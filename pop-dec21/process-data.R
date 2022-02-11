# Packages ----------------------------------------------------------------
library(tidyverse)
library(openxlsx)
library(ggplot2)
library(scales)
library(plotly)
library(odbc)
library(DBI)
library(knitr)

# Inputs ------------------------------------------------------------------
psrc_counties <- c("King","Kitsap","Pierce","Snohomish")
ofm.url <- "https://ofm.wa.gov/sites/default/files/public/dataresearch/pop/april1/ofm_april1_population_final.xlsx"
jurisdiction.tbl <- "Political.jurisdiction_dims"
server.name <- "AWS-PROD-SQL\\SOCKEYE"
database.name <- "Elmer"

psrc_colors <- c(
  "King County" = "#AD5CAB",
  "Kitsap County" = "#F4835E",
  "Pierce County" = "#A9D46E",
  "Snohomish County" = "#40BDB8",
  "Region" = "#4C4C4C")

# Jurisdiction Details ----------------------------------------------------
db.con <- dbConnect(odbc::odbc(),
                    driver = "SQL Server",
                    server = server.name,
                    database = database.name,
                    UID = Sys.getenv("userid"),
                    PWD = Sys.getenv("pwd")
)

airport.community <- c("Auburn", "Burien", "Covington", "Des Moines", "Federal Way", "Kenmore", "Kent", "Lake Forest Park",
                       "Normandy Park", "Renton", "SeaTac", "Seattle", "Skykomish", "Tukwila", "Bremerton", "Eatonville",
                       "Gig Harbor", "Lakewood", "Steilacoom", "Tacoma", "University Place", "Arlington", "Darrington",
                       "Everett", "Marysville", "Monroe","Mukilteo", "Snohomish", "Sultan")

airport.community <- enframe(airport.community) %>%
  mutate(airport_affected = "Yes") %>%
  select(-name)

jurisdictions <- as_tibble(dbReadTable(db.con,SQL(jurisdiction.tbl))) %>%
  mutate(juris_name = gsub("Seatac","SeaTac",juris_name)) %>%
  mutate(juris_name = gsub("Beau Arts Village","Beaux Arts Village",juris_name)) %>%
  select(juris_name, regional_geography, airport_affected, county_fips) %>%
  distinct() %>%
  mutate(regional_geography=gsub("HCT","High Capacity Transit Community",regional_geography)) %>%
  mutate(regional_geography=gsub("Metro","Metropolitan Cities",regional_geography)) %>%
  mutate(regional_geography=gsub("Core","Core Cities",regional_geography)) %>%
  mutate(regional_geography=gsub("CitiesTowns","Cities & Towns",regional_geography)) %>%
  select(-airport_affected)

jurisdictions <- left_join(jurisdictions, airport.community, by=c("juris_name"="value")) %>%
  mutate(across(airport_affected, ~replace_na(.x, "No"))) %>%
  mutate(county_name = case_when(
    county_fips == '033' ~ "King County",
    county_fips == '035' ~ "Kitsap County",
    county_fips == '053' ~ "Pierce County",
    county_fips == '061' ~ "Snohomish County")) %>%
  filter(!(juris_name=="Auburn" & county_fips=="053")) %>%
  filter(!(juris_name=="Bothell" & county_fips=="061")) %>%
  filter(!(juris_name=="Enumclaw" & county_fips=="053")) %>%
  filter(!(juris_name=="Milton" & county_fips=="033")) %>%
  filter(!(juris_name=="Pacific" & county_fips=="053")) %>%
  filter(!(juris_name=="Silverdale")) %>%
  filter(!(str_detect(juris_name,"Uninc."))) %>%
  select(-county_fips)
  
odbc::dbDisconnect(db.con)
rm(airport.community)

# OFM April 1st Population Data -----------------------------------------------------
download.file(ofm.url, "working.xlsx", quiet = TRUE, mode = "wb")
ofm.pop.file <- paste0(getwd(),"/working.xlsx")
ofm.pop <- as_tibble(read.xlsx(ofm.pop.file, detectDates = TRUE, skipEmptyRows = TRUE, startRow = 5, colNames = TRUE))

# Get rid of things we don't need, make it long form and clean it up for use in plot making
ofm.pop <- ofm.pop %>%
  filter(County %in%  c(psrc_counties,"State")) %>%
  pivot_longer(cols=contains("Population"), names_to="Year", values_to="Estimate") %>%
  select(-Line) %>%
  mutate(Year = str_replace(Year, ".Population.*", ""), Jurisdiction = str_replace(Jurisdiction, " \\(part\\)", "")) %>%
  mutate(across(c('Filter','Year','Estimate'), as.numeric))

# Create a Regional Summary by Filter Type and then Join to Full OFM tibble
region.pop <- ofm.pop %>%
  filter(Filter <= 3) %>%
  select(Filter,Year, Estimate) %>%
  group_by(Filter,Year) %>%
  summarize_all(sum) %>%
  mutate(County = "Region") %>%
  mutate(Jurisdiction = "Region") %>%
  mutate(Jurisdiction = ifelse(Filter == 2, "Unincorporated Region", Jurisdiction)) %>%
  mutate(Jurisdiction = ifelse(Filter == 3, "Incorporated Region", Jurisdiction))

# Add the regional results to the OFM full tibble
ofm.pop <- bind_rows(ofm.pop,region.pop) %>% mutate(Variable="Total Population")
rm(region.pop)

# Combine Jurisdictions that span multiple counties
ofm.pop <- ofm.pop %>%
  select(-County) %>%
  group_by(Filter, Jurisdiction, Year, Variable) %>%
  summarize(Estimate = sum(Estimate)) %>%
  as_tibble()

# Add Annual Change
ofm.pop <- ofm.pop %>%
  mutate(Delta = Estimate - lag(Estimate)) %>%
  mutate(Delta = case_when(Year == 2021 ~ Delta))

# Add Jurisdiction Details
ofm.pop <- left_join(ofm.pop, jurisdictions, by=c("Jurisdiction"="juris_name"))
rm(jurisdictions)

# County Summary ----------------------------------------------------------
region.2020 <- ofm.pop %>% filter(Year==2020 & Jurisdiction=="Region") %>% select(Estimate) %>% mutate(Estimate=round(Estimate,-2)) %>% pull()
region.2021 <- ofm.pop %>% filter(Year==2021 & Jurisdiction=="Region") %>% select(Estimate) %>% mutate(Estimate=round(Estimate,-2)) %>% pull()
region.delta <- ofm.pop %>% filter(Year==2021 & Jurisdiction=="Region") %>% select(Delta) %>% mutate(Delta=round(Delta,-2)) %>% pull()

county.order <- c("King County", "Kitsap County", "Pierce County", "Snohomish County", "Region")
w.data <- ofm.pop %>% filter(Year==2021 & Filter==1) %>% mutate(Jurisdiction=factor(Jurisdiction,levels = county.order))
county.growth <-  ggplot(data = w.data,
                        aes(x = Jurisdiction, y = Delta, fill = Jurisdiction, group=1)) +
                        geom_col(color = "black", alpha = 1.0, position = "dodge") +
                        geom_text(aes(label = paste0(prettyNum(round(Delta,-2), big.mark = ","))), vjust = 1.5, colour = "white", size=4) +
                        scale_y_continuous(labels = label_comma()) +
                        scale_fill_manual(values = psrc_colors) +
                        labs(x = NULL, y = NULL) +
                        theme(plot.title = element_text(size = 10, face = 'bold'),
                              axis.text.x = element_blank(),
                              axis.text.y = element_blank(),
                              axis.ticks.x = element_blank(),
                              axis.ticks.y = element_blank(),
                              panel.background = element_blank(),
                              panel.grid.major.y = element_blank(),
                              panel.grid.minor.y = element_blank(),
                              panel.grid.major.x = element_blank(),
                              panel.grid.minor.x = element_blank(),
                              legend.position = 'bottom',
                              legend.title = element_blank())

county.data <- w.data %>% select(-Filter,-regional_geography, -airport_affected, -county_name) %>% arrange(Jurisdiction)

# Unincorporated Area Growth -----------------------------------------------
incorporated.2021.delta <- ofm.pop %>% filter(Year==2021 & Jurisdiction=="Incorporated Region") %>% select(Delta) %>% mutate(Delta=round(Delta,-2)) %>% pull()
unincorporated.2021.delta <- ofm.pop %>% filter(Year==2021 & Jurisdiction=="Unincorporated Region") %>% select(Delta) %>% mutate(Delta=round(Delta,-2)) %>% pull()

incorporated.colors <- c("Incorporated Region" = "#8CC63E", "Unincorporated Region" = "#F05A28")

w.data <- ofm.pop %>% filter(Jurisdiction=="Unincorporated Region" | Jurisdiction=="Incorporated Region") %>% filter(Year==2021)

incorporated.growth <- ggplot(w.data, aes(x="", y=Delta, fill=Jurisdiction)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  scale_fill_manual(values = incorporated.colors) +
  labs(x = NULL, y = NULL) +
  theme(plot.title = element_text(size = 10, face = 'bold'),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = 'bottom',
        legend.title = element_blank())

incorporated.data <- w.data %>% select(-Filter,-regional_geography, -airport_affected, -county_name) %>% arrange(Jurisdiction)

# Regional Geography Growth -----------------------------------------------
metro.2021.delta <- ofm.pop %>% filter(Year==2021 & regional_geography=="Metropolitan Cities") %>% select(Delta) %>% mutate(Delta=round(Delta,-1)) %>% pull() %>% sum()
core.2021.delta <- ofm.pop %>% filter(Year==2021 & regional_geography=="Core Cities") %>% select(Delta) %>% mutate(Delta=round(Delta,-1)) %>% pull() %>% sum()
hct.2021.delta <- ofm.pop %>% filter(Year==2021 & regional_geography=="High Capacity Transit Community") %>% select(Delta) %>% mutate(Delta=round(Delta,-1)) %>% pull() %>% sum()
citytown.2021.delta <- ofm.pop %>% filter(Year==2021 & regional_geography=="Cities & Towns") %>% select(Delta) %>% mutate(Delta=round(Delta,-1)) %>% pull() %>% sum()

rgeo.colors <- c("Metropolitan Cities" = "#91268F", 
                "Core Cities" = "#F05A28",
                "High Capacity Transit Community" = "#8CC63E",
                "Cities & Towns" = "#00A7A0")

w.data <- ofm.pop %>% 
  filter(regional_geography %in% c("Metropolitan Cities", "Core Cities", "High Capacity Transit Community", "Cities & Towns") & Year==2021) %>%
  select(regional_geography, Delta) %>%
  group_by(regional_geography) %>%
  summarize(Delta=sum(Delta)) %>%
  as_tibble()
    
rgeo.order <- c("Metropolitan Cities", "Core Cities", "High Capacity Transit Community", "Cities & Towns")
w.data <- w.data %>% mutate(regional_geography=factor(regional_geography,levels = rgeo.order))    
    
rgeo.growth <-  ggplot(data = w.data,
                        aes(x = regional_geography, y = Delta, fill = regional_geography, group=1)) +
                geom_col(color = "black", alpha = 1.0, position = "dodge") +
                geom_text(aes(label = paste0(prettyNum(round(Delta,-2), big.mark = ","))), vjust = 1.5, colour = "white", size=4) +
                scale_y_continuous(labels = label_comma()) +
                scale_fill_manual(values = rgeo.colors) +
                labs(x = NULL, y = NULL) +
                theme(plot.title = element_text(size = 10, face = 'bold'),
                  axis.text.x = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.ticks.y = element_blank(),
                  panel.background = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  legend.position = 'bottom',
                  legend.title = element_blank())

rgeo.data <- w.data %>% mutate(Year=2021, Variable="Total Population") %>% arrange(regional_geography)

# Top Ten Cities in Region ------------------------------------------------------
city.colors <- c(
  "Arlington" = "#630460",
  "Auburn" = "#9f3913",
  "Bellevue" = "#588527",
  "Bonney\nLake" = "#00716c",
  "Edgewood" = "#3e4040",
  "Everett" = "#AD5CAB",
  "Kent" = "#F4835E",
  "Kirkland" = "#A9D46E",
  "Lake\nStevens" = "#40BDB8",
  "Seattle" = "#76787A")

w.data <- ofm.pop %>% filter(Year==2021 & Filter==4) %>% arrange(desc(Delta)) %>% slice(1:10)
w.data$Jurisdiction <- gsub(" ", "\n", w.data$Jurisdiction)

king.cities.top <- w.data %>% select(county_name) %>% filter(county_name=="King County") %>% pull() %>% length()
pierce.cities.top <- w.data %>% select(county_name) %>% filter(county_name=="Pierce County") %>% pull() %>% length()
snohomish.cities.top <- w.data %>% select(county_name) %>% filter(county_name=="Snohomish County") %>% pull() %>% length()

top.ten.cities <-  ggplot(data = w.data,
                          aes(x = reorder(Jurisdiction, -Delta), y = Delta, fill = Jurisdiction, group=1)) +
  geom_col(color = "black", alpha = 1.0, position = "dodge") +
  geom_text(aes(label = paste0(Jurisdiction,"\n", prettyNum(round(Delta,-1), big.mark = ","))), vjust = 1.0, colour = "white", size=3) +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_manual(values = city.colors) +
  labs(x = NULL, y = NULL) +
  theme(plot.title = element_text(size = 10, face = 'bold'),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = 'none',
        legend.title = element_blank())

region.fastest.data <- ofm.pop %>% filter(Year==2021 & Filter==4) %>% arrange(desc(Delta)) %>% slice(1:10) %>% select(-Filter)

# Top Ten Cities in King County -------------------------------------------
city.colors <- c(
  "Auburn" = "#630460",
  "Bellevue" = "#9f3913",
  "Black Diamond" = "#588527",
  "Federal\nWay" = "#00716c",
  "Kent" = "#3e4040",
  "Kirkland" = "#AD5CAB",
  "Maple\nValley" = "#F4835E",
  "Redmond" = "#A9D46E",
  "Seattle" = "#40BDB8",
  "Shoreline" = "#76787A")

w.data <- ofm.pop %>% filter(Year==2021 & Filter==4 & county_name=="King County") %>% arrange(desc(Delta)) %>% slice(1:10)
w.data$Jurisdiction <- gsub(" ", "\n", w.data$Jurisdiction)

top.ten.cities.king <-  ggplot(data = w.data,
                          aes(x = reorder(Jurisdiction, -Delta), y = Delta, fill = Jurisdiction, group=1)) +
  geom_col(color = "black", alpha = 1.0, position = "dodge") +
  geom_text(aes(label = paste0(Jurisdiction,"\n", prettyNum(round(Delta,-1), big.mark = ","))), vjust = 1.0, colour = "white", size=3) +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_manual(values = city.colors) +
  labs(x = NULL, y = NULL) +
  theme(plot.title = element_text(size = 10, face = 'bold'),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = 'none',
        legend.title = element_blank())

king.fastest.data <- ofm.pop %>% filter(Year==2021 & Filter==4 & county_name=="King County") %>% arrange(desc(Delta)) %>% slice(1:10) %>% select(-Filter)

# Top Cities in Kitsap County -------------------------------------------
city.colors <- c(
  "Bainbridge\nIsland" = "#630460",
  "Bremerton" = "#9f3913",
  "Port\nOrchard" = "#588527",
  "Poulsbo" = "#00716c")

w.data <- ofm.pop %>% filter(Year==2021 & Filter==4 & county_name=="Kitsap County") %>% arrange(desc(Delta)) %>% slice(1:10)
w.data$Jurisdiction <- gsub(" ", "\n", w.data$Jurisdiction)

top.ten.cities.kitsap <-  ggplot(data = w.data,
                               aes(x = reorder(Jurisdiction, -Delta), y = Delta, fill = Jurisdiction, group=1)) +
  geom_col(color = "black", alpha = 1.0, position = "dodge") +
  geom_text(aes(label = paste0(Jurisdiction,"\n", prettyNum(round(Delta,-1), big.mark = ","))), vjust = 1.0, colour = "white", size=3) +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_manual(values = city.colors) +
  labs(x = NULL, y = NULL) +
  theme(plot.title = element_text(size = 10, face = 'bold'),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = 'none',
        legend.title = element_blank())

kitsap.fastest.data <- ofm.pop %>% filter(Year==2021 & Filter==4 & county_name=="Kitsap County") %>% arrange(desc(Delta)) %>% slice(1:10) %>% select(-Filter)

# Top Ten Cities in Pierce County -------------------------------------------
city.colors <- c(
  "Bonney\nLake" = "#630460",
  "Buckley" = "#9f3913",
  "Eatonville" = "#588527",
  "Edgewood" = "#00716c",
  "Fife" = "#3e4040",
  "Fircrest" = "#AD5CAB",
  "Gig\nHarbor" = "#F4835E",
  "Puyallup" = "#A9D46E",
  "Sumner" = "#40BDB8",
  "University\nPlace" = "#76787A")

w.data <- ofm.pop %>% filter(Year==2021 & Filter==4 & county_name=="Pierce County") %>% arrange(desc(Delta)) %>% slice(1:10)
w.data$Jurisdiction <- gsub(" ", "\n", w.data$Jurisdiction)

top.ten.cities.pierce <-  ggplot(data = w.data,
                               aes(x = reorder(Jurisdiction, -Delta), y = Delta, fill = Jurisdiction, group=1)) +
  geom_col(color = "black", alpha = 1.0, position = "dodge") +
  geom_text(aes(label = paste0(Jurisdiction,"\n", prettyNum(round(Delta,-1), big.mark = ","))), vjust = 1.0, colour = "white", size=3) +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_manual(values = city.colors) +
  labs(x = NULL, y = NULL) +
  theme(plot.title = element_text(size = 10, face = 'bold'),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = 'none',
        legend.title = element_blank())

pierce.fastest.data <- ofm.pop %>% filter(Year==2021 & Filter==4 & county_name=="Pierce County") %>% arrange(desc(Delta)) %>% slice(1:10) %>% select(-Filter)

# Top Ten Cities in Snohomish County -------------------------------------------
city.colors <- c(
  "Arlington" = "#630460",
  "Edmonds" = "#9f3913",
  "Everett" = "#588527",
  "Lake\nStevens" = "#00716c",
  "Lynnwood" = "#3e4040",
  "Marysville" = "#AD5CAB",
  "Monroe" = "#F4835E",
  "Mountlake\nTerrace" = "#A9D46E",
  "Stanwood" = "#40BDB8",
  "Sultan" = "#76787A")

w.data <- ofm.pop %>% filter(Year==2021 & Filter==4 & county_name=="Snohomish County") %>% arrange(desc(Delta)) %>% slice(1:10)
w.data$Jurisdiction <- gsub(" ", "\n", w.data$Jurisdiction)

top.ten.cities.snohomish <-  ggplot(data = w.data,
                                 aes(x = reorder(Jurisdiction, -Delta), y = Delta, fill = Jurisdiction, group=1)) +
  geom_col(color = "black", alpha = 1.0, position = "dodge") +
  geom_text(aes(label = paste0(Jurisdiction,"\n", prettyNum(round(Delta,-1), big.mark = ","))), vjust = 1.0, colour = "white", size=3) +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_manual(values = city.colors) +
  labs(x = NULL, y = NULL) +
  theme(plot.title = element_text(size = 10, face = 'bold'),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = 'none',
        legend.title = element_blank())

snohomish.fastest.data <- ofm.pop %>% filter(Year==2021 & Filter==4 & county_name=="Snohomish County") %>% arrange(desc(Delta)) %>% slice(1:10) %>% select(-Filter)

# City Population Table ---------------------------------------------------
w.data.20 <- ofm.pop %>% 
  filter(Year==2020 & Filter==4) %>% 
  select(Jurisdiction, county_name, Estimate) %>%
  rename(`Population 2020`=Estimate)

w.data.21 <- ofm.pop %>% 
  filter(Year==2021 & Filter==4) %>% 
  select(Jurisdiction, Estimate, Delta) %>%
  rename(`Population 2021`=Estimate)

city.pop.data <- left_join(w.data.20,w.data.21,by="Jurisdiction") %>% 
  rename(`County`= county_name) %>%
  mutate(`Population 2020` = round(`Population 2020`,-1), `Population 2021` = round(`Population 2021`,-1), Delta = round(Delta,-1))

city.growth.tbl <- kable(city.pop.data, align = "llccc", format.args = list(decimal.mark = ".", big.mark = ","), format="simple")

# Write Data to Excel for Trend Production --------------------------------
l <-list("county-growth" = county.data, "incorporated-growth" = incorporated.data, "regional-geography-growth" = rgeo.data,
         "region-fastest-growing" = region.fastest.data, "king-fastest-growing" = king.fastest.data, "kitsap-fastest-growing" = kitsap.fastest.data,
         "pierce-fastest-growing" = pierce.fastest.data, "snohomish-fastest-growing" = snohomish.fastest.data,
         "city-data" = city.pop.data)

write.xlsx(l, "pop-data-for-ofm-trend.xlsx", overwrite=TRUE)
