# Packages ----------------------------------------------------------------

# Data Cleaning/Processing
library(tidyr)
library(dplyr)
library(stringr)
library(openxlsx)
library(lubridate)
library(pdftools)
library(data.table)

# Chart creation
library(ggplot2)
library(scales)
library(plotly)

# Spatial Data
library(sf)


# URL's for auto downloads ---------------------------------------------------------
ofm.url <- "https://ofm.wa.gov/sites/default/files/public/dataresearch/pop/april1/ofm_april1_population_final.xlsx"
ofm.change.url <- "https://ofm.wa.gov/sites/default/files/public/dataresearch/pop/components/ofm_april1_components_of_change_1960_to_present.xlsx"
ofm.housing.url <- "https://ofm.wa.gov/sites/default/files/public/dataresearch/pop/april1/hseries/ofm_april1_postcensal_estimates_housing_1980_1990-present.xlsx"

esd.monthly.url <- 'https://esdorchardstorage.blob.core.windows.net/esdwa/Default/ESDWAGOV/labor-market-info/Libraries/Economic-reports/Washington-employment-estimates/WA-QB-historical-SA.xlsx'
esd.unemployment.url <- "https://esdorchardstorage.blob.core.windows.net/esdwa/Default/ESDWAGOV/newsroom/Statistics/wkly-initial-claims-count-table.pdf"
esd.continued.claims.url <- "https://media.esd.wa.gov/esdwa/Default/ESDWAGOV/labor-market-info/Libraries/Regional-reports/UI-Claims-Karen/COVID19%20Docs/Continued%20Claims%20Demographics%20Published-30.xlsx"

# Files manually download ---------------------------
nonhispanic.url <- "X:/DSA/shiny-uploads/trends/rsc-jul21/data/workers-by-race-quarter.xlsx"
hispanic.url <- "X:/DSA/shiny-uploads/trends/rsc-jul21/data/workers-by-hispanic-origin-race.xlsx"

sales.url <- "X:/DSA/shiny-uploads/trends/rsc-jul21/data/TaxableRetailSalesJuly21.xlsx"

homesales.url <- "X:/DSA/shiny-uploads/trends/rsc-jul21/data/HomeSalesRedfinJuly21.xlsx"
rentals.url <- "X:/DSA/shiny-uploads/trends/rsc-jul21/data/Metro_ZORI_AllHomesPlusMultifamily_SSA.csv"

ntd.url <- "X:/DSA/shiny-uploads/trends/rsc-jul21/data/May-2021-Raw-Database.xlsx"
volume_file <- "X:/DSA/shiny-uploads/trends/rsc-jul21/data/VolumeNumTableCountLocation_data.csv"
truck_file <- "X:/DSA/shiny-uploads/trends/rsc-jul21/data/Freight_Table_data.csv"
ferry_file <- "X:/DSA/shiny-uploads/trends/rsc-jul21/data/FerriesTable_data.csv"

# Variables and Lists -----------------------------------------------------
starting.year <- 2010

psrc_counties <- c("King","Kitsap","Pierce","Snohomish")

psrc.transit <- c("1","3","5","20","23","29","35","40","54")

transit.agency.order <- c("Everett Transit", "Kitsap Transit", "City of Seattle", "Pierce Transit", "Community Transit", "Sound Transit", "King County Metro", "Washington State Ferries")

bus.modes <- c("CB","MB","TB")
ferry.modes <- c("FB")
light.rail.modes <- c("LR","SR","MG","MO")
commuter.rail.modes <- c("CR")

psrc_colors <- c(
  "King County" = "#AD5CAB",
  "Incorporated King County" = "#C388C2",
  "Unincorporated King County" = "#E3C9E3",
  "Kitsap County" = "#F4835E",
  "Incorporated Kitsap County" = "#F7A489",
  "Unincorporated Kitsap County" = "#FBD6C9",
  "Pierce County" = "#A9D46E",
  "Incorporated Pierce County" = "#C0E095",
  "Unincorporated Pierce County" = "#E2F1CF",
  "Snohomish County" = "#40BDB8",
  "Incorporated Snohomish County" = "#73CFCB",
  "Unincorporated Snohomish County" = "#BFE9E7",
  "Region Total" = "#91268F",
  "Incorporated Region Total" = "#AD5CAB",
  "Unincorporated Region Total" = "#E3C9E3",
  "State Total" = "#8CC63E",
  "Incorporated State Total" = "#A9D46E",
  "Unincorporated State Total" = "#E2F1CF",
  "Thurston County" = "#4C4C4C",
  "Incorporated Thurston County" = "#999999",
  "Unincorporated Thurston County" = "#BCBEC0",
  "Washington State" = "#999999",
  "Seattle MSA" = "#C388C2",
  "Tacoma MSA" = "#F7A489",
  "Bremerton MSA" = "#C0E095",
  "Region" = "#73CFCB",
  "Migration" = "#40BDB8",
  "Natural Increase" = "#A9D46E",
  "Total Nonfarm" = "#8CC63E",
  "Initial Claims" = "#AD5CAB",
  "4 Week Average" = "#C388C2",
  "Sales" = "#588527",
  "Use" = "#C0E095",
  "Median Sales Price" = "#F4835E",
  "Homes Sold" = "#AD5CAB",
  "Home Inventory" = "#A9D46E",
  "Seattle, WA" = "#00A7A0",
  "Single-Family" = "#8CC63E",
  "Multi-Family" = "#F05A28",
  "Mobile-Home" = "#91268F",
  "Black or African American Alone" = "#91268F",
  "American Indian or Alaska Native Alone" = "#8CC63E",
  "Asian Alone" = "#00A7A0",
  "White Alone" = "#76787A",
  "Hispanic or Latino" = "#F05A28",
  "Native Hawaiian or Other Pacific Islander Alone" = "#AD5CAB",
  "Two or More Race Groups" = "#A9D46E",
  "Sound Transit" = "#005753",
  "King County Metro" = "#9f3913",
  "Community Transit" = "#8CC63E",
  "Kitsap Transit" = "#91268F",
  "Everett Transit" = "#C0E095",
  "Pierce Transit" = "#C388C2",
  "Washington State Ferries" = "#00A7A0",
  "City of Seattle" = "#F7A489"
)

city.class.order <- c("Metro","Core","HCT","CitiesTowns","Unincorporated County")

# Spatial Layers ----------------------------------------------------------
geodatabase.server <- "AWS-PROD-SQL\\Sockeye"
geodatabase.name <- "ElmerGeo"
gdb.nm <- paste0("MSSQL:server=",geodatabase.server,";database=",geodatabase.name,";trusted_connection=yes")
spn <- 2285
wgs84 <- 4326

city.lyr <- st_read(gdb.nm, "dbo.cities", crs = spn)
regeo <- city.lyr %>% st_drop_geometry() %>% select(city_name,class_desc)

# Functions ---------------------------------------------------------------

create_bar_chart <- function(w.data, w.x, w.y, w.bartype, w.transparent, w.color, w.palette=psrc_colors, w.month.breaks, w.date.format, w.dec=0, w.suff="") {  
  
    x.breaks <- unique(w.data %>% pull(w.x))
    
    g <-  ggplotly(ggplot(data = w.data,
                          aes(x = get(eval(w.x)), 
                              y = get(eval(w.y)), 
                              fill = get(eval(w.color)),
                              group=1, 
                              text = paste0("<b>", get(eval(w.color)),": </b>", prettyNum(round(get(eval(w.y)), w.dec), big.mark = ","), w.suff,"<br>"))) +
                    geom_col(color = "black",
                              alpha = w.transparent,
                              position = w.bartype) +
                    scale_y_continuous(labels = label_comma()) +
                    scale_fill_manual(values = w.palette) +
                    scale_x_date(breaks = w.month.breaks, labels = date_format(w.date.format)) +
                    labs(x = NULL, y = NULL) +
                    theme(plot.title = element_text(size = 10, face = 'bold'),
                         axis.text.x = element_text(angle = 0,
                                                    hjust = 1, 
                                                    vjust = 1,
                                                    family = 'Comic Sans MS'),
                         axis.ticks.x = element_blank(),
                         axis.line = element_blank(),
                         panel.background = element_blank(),
                         panel.grid.major.y = element_line(colour="#BBBDC0",size = 0.25),
                         panel.grid.minor.y = element_line(colour="#BBBDC0",size = 0.25),
                         panel.grid.major.x = element_blank(),
                         panel.grid.minor.x = element_blank(),
                         text = element_text(family = "Segoe UI"),
                         legend.position = "bottom",
                         legend.title = element_blank()),
                  tooltip = c("text")) %>% layout(legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.1), hovermode = "x")

}

create_line_chart <- function(w_tbl, w_title, w_label, w_dec, w_colors, w_group, w_factor, w_suff) {
  
  w_chart <- ggplotly(ggplot(data=w_tbl, aes(y=Ratio, x=Date, group=get(w_group), color=factor(get(w_group)),text = paste0(month(`Date`),"-",day(`Date`),"-",year(`Date`)," ",w_title,": ",prettyNum(round(Ratio*w_factor, w_dec), big.mark = ","),w_suff)))+
                        geom_line(size=1.2) + 
                        #scale_color_manual(values=w_colors)+
                        scale_x_date(labels = date_format("%B")) +
                        scale_y_continuous(labels = w_label) +
                        ylab(w_title)+
                        theme_light() +
                        theme(
                          axis.text=element_text(size=10),
                          axis.text.x.bottom=element_text(size=10),
                          axis.title.y =element_text(size=10,face="bold"),
                          axis.title.x = element_blank(),
                          panel.grid.major = element_line(colour="#BBBDC0",size = 0.25),
                          panel.grid.minor = element_line(colour="#BBBDC0",size = 0.25),
                          panel.border = element_blank(),
                          axis.line = element_blank(),
                          legend.position="bottom",
                          legend.title = element_blank())
                      ,tooltip = c("text")) %>% layout(legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.1), hovermode = "x")
  
  
  return(w_chart)
}


# OFM Population Data -----------------------------------------------------
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
  mutate(Jurisdiction = "Region Total") %>%
  mutate(Jurisdiction = ifelse(Filter == 2, "Unincorporated Region Total", Jurisdiction)) %>%
  mutate(Jurisdiction = ifelse(Filter == 3, "Incorporated Region Total", Jurisdiction))

# Add the regional results to the OFM full tibble
ofm.pop <- bind_rows(ofm.pop,region.pop) %>% mutate(Variable="Total Population")

# Process Population change components - Natural Increase
download.file(ofm.change.url, "working.xlsx", quiet = TRUE, mode = "wb")
ofm.pop.chg.file <- paste0(getwd(),"/working.xlsx")

ofm.pop.chg.natural <- as_tibble(read.xlsx(ofm.pop.chg.file, detectDates = TRUE, skipEmptyRows = TRUE, startRow = 4, colNames = TRUE, sheet = "Natural Increase"))

# Get rid of things we don't need, make it long form and clean it up for use in plot making
ofm.pop.chg.natural <- ofm.pop.chg.natural %>%
  filter(County %in%  c(psrc_counties,"State")) %>%
  pivot_longer(cols=contains("Natural.Increase"), names_to="Year", values_to="Estimate") %>%
  select(-Spacer) %>%
  mutate(Year = str_replace(Year, "Natural.Increase.", ""), Jurisdiction = str_replace(Jurisdiction, " \\(part\\)", "")) %>%
  separate(Year, c("temp", "Year"), "-") %>%
  select(-temp) %>%
  mutate(Year = str_replace(Year, "20211", "2021")) %>%
  mutate(across(c('Year','Estimate'), as.numeric))

# Create a Regional Summary by Filter Type and then Join to Full OFM tibble
region.pop <- ofm.pop.chg.natural %>%
  filter(County != "State") %>%
  select(Year, Estimate) %>%
  group_by(Year) %>%
  summarize_all(sum) %>%
  mutate(County = "Region") %>%
  mutate(Jurisdiction = "Region Total")

# Add the regional results to the OFM full tibble
ofm.pop.chg.natural <- bind_rows(ofm.pop.chg.natural,region.pop) %>% mutate(Variable="Natural Increase")

# Process Population change components - Migration
download.file(ofm.change.url, "working.xlsx", quiet = TRUE, mode = "wb")
ofm.pop.chg.file <- paste0(getwd(),"/working.xlsx")

ofm.pop.chg.migration <- as_tibble(read.xlsx(ofm.pop.chg.file, detectDates = TRUE, skipEmptyRows = TRUE, startRow = 4, colNames = TRUE, sheet = "Residual Net Migration"))

# Get rid of things we don't need, make it long form and clean it up for use in plot making
ofm.pop.chg.migration <- ofm.pop.chg.migration %>%
  filter(County %in%  c(psrc_counties,"State")) %>%
  pivot_longer(cols=contains("Residual.Net.Migration"), names_to="Year", values_to="Estimate") %>%
  select(-Spacer) %>%
  mutate(Year = str_replace(Year, "Residual.Net.Migration.", ""), Jurisdiction = str_replace(Jurisdiction, " \\(part\\)", "")) %>%
  separate(Year, c("temp", "Year"), "-") %>%
  select(-temp) %>%
  mutate(Year = str_replace(Year, "20211", "2021")) %>%
  mutate(across(c('Year','Estimate'), as.numeric))

# Create a Regional Summary by Filter Type and then Join to Full OFM tibble
region.pop <- ofm.pop.chg.migration %>%
  filter(County != "State") %>%
  select(Year, Estimate) %>%
  group_by(Year) %>%
  summarize_all(sum) %>%
  mutate(County = "Region") %>%
  mutate(Jurisdiction = "Region Total")

# Add the regional results to the OFM full tibble
ofm.pop.chg.migration <- bind_rows(ofm.pop.chg.migration,region.pop) %>% mutate(Variable="Migration")

# Process Population change components - Total Change
download.file(ofm.change.url, "working.xlsx", quiet = TRUE, mode = "wb")
ofm.pop.chg.file <- paste0(getwd(),"/working.xlsx")

ofm.pop.chg.total <- as_tibble(read.xlsx(ofm.pop.chg.file, detectDates = TRUE, skipEmptyRows = TRUE, startRow = 4, colNames = TRUE, sheet = "Annual Change"))

# Get rid of things we don't need, make it long form and clean it up for use in plot making
ofm.pop.chg.total <- ofm.pop.chg.total %>%
  filter(County %in%  c(psrc_counties,"State")) %>%
  pivot_longer(cols=contains("Annual.Change.in.Total.Population"), names_to="Year", values_to="Estimate") %>%
  select(-Spacer) %>%
  mutate(Year = str_replace(Year, "Annual.Change.in.Total.Population.", ""), Jurisdiction = str_replace(Jurisdiction, " \\(part\\)", "")) %>%
  separate(Year, c("temp", "Year"), "-") %>%
  select(-temp) %>%
  mutate(Year = str_replace(Year, "20211", "2021")) %>%
  mutate(across(c('Year','Estimate'), as.numeric))

# Create a Regional Summary by Filter Type and then Join to Full OFM tibble
region.pop <- ofm.pop.chg.total %>%
  filter(County != "State") %>%
  select(Year, Estimate) %>%
  group_by(Year) %>%
  summarize_all(sum) %>%
  mutate(County = "Region") %>%
  mutate(Jurisdiction = "Region Total")

# Add the regional results to the OFM full tibble
ofm.pop.chg.total <- bind_rows(ofm.pop.chg.total,region.pop) %>% mutate(Variable="Total Change")

# Combine components of change into one tibble
ofm.population.change <- bind_rows(list(ofm.pop.chg.natural, ofm.pop.chg.migration, ofm.pop.chg.total))

# Region Summary
region.pop <- ofm.population.change %>% filter(Jurisdiction=="Region Total", Variable !="Total Change", Year >=starting.year) %>% select(Jurisdiction, Year, Estimate, Variable)
region.pop$Year <- as.Date(paste(region.pop$Year, 4, 1, sep = "-"))
region.pop.change.chart <- create_bar_chart(w.data = region.pop, w.x = "Year", w.y = "Estimate", w.bartype = "stack", w.transparent = 1.0, w.color = "Variable", w.month.breaks= "1 year", w.date.format = "%Y")

pop.2020 <- region.pop %>% filter(year(Year) == 2020) %>% select(Jurisdiction, Estimate) %>% group_by(Jurisdiction) %>% summarize_all(sum) %>% select(Estimate) %>% pull()
pop.2021 <- region.pop %>% filter(year(Year) == 2021) %>% select(Jurisdiction, Estimate) %>% group_by(Jurisdiction) %>% summarize_all(sum) %>% select(Estimate) %>% pull()

# Housing Units
download.file(ofm.housing.url, "working.xlsx", quiet = TRUE, mode = "wb")
ofm.housing.file <- paste0(getwd(),"/working.xlsx")
ofm.housing <- as_tibble(read.xlsx(ofm.housing.file, sheet = "Housing Units", detectDates = TRUE, skipEmptyRows = TRUE, startRow = 4, colNames = TRUE))

# Get rid of things we don't need, make it long form and clean it up for use in plot making
ofm.housing <- ofm.housing %>%
  filter(County %in%  c(psrc_counties)) %>%
  pivot_longer(cols=contains(".Housing.Units"), names_to="Year", values_to="Estimate") %>%
  select(-Line) %>%
  mutate(Year = str_replace(Year, ".Census.Count.of.", "-"), Year = str_replace(Year, ".Postcensal.Estimate.of.", "-"), Year = str_replace(Year, ".Census-Based.Estimate.of.", "-")) %>%
  separate(Year, c("Year", "Variable"), "-") %>%
  mutate(across(c('Year'), as.numeric)) %>%
  filter(Year>=2000) %>%
  mutate(across(c('Estimate'), as.numeric))

temp <- ofm.housing %>% filter(Variable=="Total.Housing.Units")

region.total.housing <- temp %>%
  filter(Filter == "1") %>%
  select(Year, Estimate) %>%
  group_by(Year) %>%
  summarize_all(sum) %>% 
  mutate(Delta = Estimate - lag(Estimate)) %>%
  mutate(Variable="Total Housing Units")

temp <- ofm.housing %>% filter(Variable=="One.Unit.Housing.Units")

region.sf.housing <- temp %>%
  filter(Filter == "1") %>%
  select(Year, Estimate) %>%
  group_by(Year) %>%
  summarize_all(sum) %>% 
  mutate(Delta = Estimate - lag(Estimate)) %>%
  mutate(Variable="Single-Family")

temp <- ofm.housing %>% filter(Variable=="Two.or.More.Housing.Units")

region.mf.housing <- temp %>%
  filter(Filter == "1") %>%
  select(Year, Estimate) %>%
  group_by(Year) %>%
  summarize_all(sum) %>% 
  mutate(Delta = Estimate - lag(Estimate)) %>%
  mutate(Variable="Multi-Family")

temp <- ofm.housing %>% filter(Variable=="Mobile.Home.and.Special.Housing.Units")

region.mh.housing <- temp %>%
  filter(Filter == "1") %>%
  select(Year, Estimate) %>%
  group_by(Year) %>%
  summarize_all(sum) %>% 
  mutate(Delta = Estimate - lag(Estimate)) %>%
  mutate(Variable="Mobile-Home")

region.housing <- bind_rows(list(region.total.housing, region.sf.housing, region.mf.housing, region.mh.housing))
region.housing$Year <- as.Date(paste(region.housing$Year, 4, 1, sep = "-"))
region.housing <- region.housing %>% filter(Variable != "Total Housing Units", year(Year) >= starting.year+1)

region.housing.chart <- create_bar_chart(w.data = region.housing, w.x = "Year", w.y = "Delta", w.bartype = "stack", w.transparent = 1.0, w.color = "Variable", w.month.breaks= "1 year", w.date.format = "%Y")

# Remove the temporary Excel File that was downloaded and remove unnecessary data from memory
file.remove(ofm.pop.file)
rm(region.pop, temp)


# ESD Monthly Wage and Salary Employment Data -----------------------------
summary.geography <- c("Seattle MSA", "Tacoma MSA", "Bremerton MSA", "Washington State")

# Download ESD File for processing
download.file(esd.monthly.url, "working.xlsx", quiet = TRUE, mode = "wb")
esd.file <- paste0(getwd(),"/working.xlsx")

# Cycle through tabs to create one master long form tibble
msa.jobs <- NULL

for (current.geography in summary.geography) {
  
  working <- as_tibble(read.xlsx(esd.file, sheet = current.geography, detectDates = TRUE, skipEmptyRows = TRUE, startRow = 2, colNames = TRUE))
  working <- working %>% 
    pivot_longer(cols=!contains("NAICS"), names_to="DataDate", values_to="Estimate") %>%
    mutate(across(c('DataDate'), as.Date)) %>%
    mutate(Geography = current.geography) %>%
    select(-NAICS.CELL)
  
  if(is.null(msa.jobs)) {msa.jobs <- working} else {msa.jobs <- bind_rows(msa.jobs,working)}
  
}

# Create a Regional Job Summary
region.jobs <- msa.jobs %>%
  filter(!Geography == "Washington State") %>%
  select(!Geography) %>%
  group_by(NAICS.INDUSTRY, DataDate) %>%
  summarize_all(sum) %>%
  mutate(Geography = "Region")

# Add the regional results to the full tibble
msa.jobs <- bind_rows(msa.jobs,region.jobs) %>% rename(Month=DataDate)

msa.jobs <- msa.jobs %>% filter(Month!="2021-06-01")
latest.month <- 5

# Delete the temporary downloaded file
file.remove(esd.file)
rm(working,region.jobs)

region.jobs <- msa.jobs %>% filter(NAICS.INDUSTRY == "Total Nonfarm", year(Month) >= starting.year, month(Month) == latest.month, Geography=="Region") %>% mutate(Delta = Estimate - lag(Estimate, order_by = Month)) %>% mutate(Year = year(Month))
region.jobs.monthly <- msa.jobs %>% filter(NAICS.INDUSTRY == "Total Nonfarm", year(Month) >= 2020, Geography=="Region") %>% mutate(Delta = Estimate - lag(Estimate, order_by = Month)) %>% mutate(Year = year(Month))

region.job.total.chart <- create_bar_chart(w.data = region.jobs, w.x = "Month", w.y = "Estimate", w.bartype = "dodge", w.transparent = 1.0, w.color = "NAICS.INDUSTRY", w.month.breaks= "1 year", w.date.format = "%Y")
region.job.change.monthly.chart <- create_bar_chart(w.data = region.jobs.monthly, w.x = "Month", w.y = "Delta", w.bartype = "dodge", w.transparent = 1.0, w.color = "NAICS.INDUSTRY", w.month.breaks= "1 month", w.date.format = "%b")

# ESD Monthly Unemployment Data --------------------------------------------
# Clean up the raw pdf 
unemployment <- pdf_text(esd.unemployment.url) %>% readr::read_lines()
unemployment <-  unemployment[7:58]
unemployment_claims <- setDT(as.data.frame(unemployment))
unemployment_claims$unemployment <- gsub("\\s+", " ",unemployment_claims$unemployment)
unemployment_claims$unemployment <- gsub(",", "",unemployment_claims$unemployment)
unemployment_claims$unemployment <- trimws(unemployment_claims$unemployment, "l")
unemployment_claims <- unemployment_claims %>% separate(unemployment, c("v1", "v2","v3", "v4", "v5", "v6", "v7","v8", "v9", "v10"), sep=" ")

# Process the 2019 data into a usable table for plotting
working <- unemployment_claims[,c(1:6)]
nms <- c("date","Initial Claims","Weekly Change", "Percent Change", "4 Week Average","day")
setnames(working,nms)
working$date <- mdy(working$date)
working$day <- mdy(working$day)
working$`Percent Change` <- as.character((as.numeric(working$`Percent Change`))/100)
unemployment_2019 <- melt(working, id.vars=c("date","day"))
unemployment_2019$value <- as.numeric(unemployment_2019$value)

# Process the 2020 data into a usable table for plotting
working <- unemployment_claims[,c(6:10)]
nms <- c("date","Initial Claims","Weekly Change", "Percent Change", "4 Week Average")
setnames(working,nms)
working$date <- mdy(working$date)
working$day <- mdy(working$day)
working$day <- working$date
working$`Percent Change` <- as.character((as.numeric(working$`Percent Change`))/100)
unemployment_2020 <- melt(working, id.vars=c("date","day"))
unemployment_2020$value <- as.numeric(unemployment_2020$value)

# Combine 2019 and 2020
unemployment <- rbind(unemployment_2019,unemployment_2020)
unemployment$year <- year(unemployment$date)
unemployment <- unemployment[variable %in% c("Initial Claims","Weekly Change", "4 Week Average")]

intial.claims <- unemployment %>% filter(variable=="Initial Claims") 
intial.claims <- na.omit(intial.claims)
initial.claims.chart <- create_bar_chart(w.data = intial.claims, w.x = "date", w.y = "value", w.bartype = "dodge", w.transparent = 1.0, w.color = "variable", w.month.breaks= "3 months", w.date.format = "%b-%Y")

avg.claims <- unemployment %>% filter(variable=="4 Week Average") 
avg.claims <- na.omit(avg.claims)
avg.claims.chart <- create_bar_chart(w.data = avg.claims, w.x = "date", w.y = "value", w.bartype = "dodge", w.transparent = 1.0, w.color = "variable", w.month.breaks= "3 months", w.date.format = "%m/%Y")


# ESD Continuing Claims by Race -------------------------------------------
download.file(esd.continued.claims.url, "working.xlsx", quiet = TRUE, mode = "wb")
cont.claims.file <- paste0(getwd(),"/working.xlsx")

king.cont.claims <- as_tibble(read.xlsx(cont.claims.file, sheet = "King", detectDates = TRUE, skipEmptyRows = TRUE, startRow = 6, colNames = TRUE)) %>% slice(7:17) %>% mutate_at(vars(contains('Week')), as.numeric) %>% mutate(County="King")
kitsap.cont.claims <- as_tibble(read.xlsx(cont.claims.file, sheet = "Kitsap", detectDates = TRUE, skipEmptyRows = TRUE, startRow = 6, colNames = TRUE)) %>% slice(7:17) %>% mutate_at(vars(contains('Week')), as.numeric) %>% mutate(County="Kitsap")
pierce.cont.claims <- as_tibble(read.xlsx(cont.claims.file, sheet = "Pierce", detectDates = TRUE, skipEmptyRows = TRUE, startRow = 6, colNames = TRUE)) %>% slice(7:17) %>% mutate_at(vars(contains('Week')), as.numeric) %>% mutate(County="Pierce")
snohomish.cont.claims <- as_tibble(read.xlsx(cont.claims.file, sheet = "Snohomish", detectDates = TRUE, skipEmptyRows = TRUE, startRow = 6, colNames = TRUE)) %>% slice(7:17) %>% mutate_at(vars(contains('Week')), as.numeric) %>% mutate(County="Snohomish")

continuing.claims <- bind_rows(list(king.cont.claims,kitsap.cont.claims,pierce.cont.claims,snohomish.cont.claims))

continuing.claims <- continuing.claims %>% 
  select(-Demographic) %>% rename(Race=Industry) %>% 
  filter(Race!="Not Latino/Hispanic:", Race!="Not Disclosed") %>%
  pivot_longer(cols= contains("Week"), names_to="Date", values_to="Estimate") %>%
  mutate(Date = gsub(".*ending.","",Date), Date = gsub("\\."," ",Date)) %>%
  mutate(Date = mdy(Date)) %>%
  mutate(Race = gsub(", All Unduplicated Claimants","",Race), Race = trimws(Race, "l"))

region.continuing.claims <- continuing.claims %>%
  select(-County) %>%
  group_by(Race,Date) %>%
  summarize_all(sum) %>%
  mutate(Race = gsub("African American","Black or African American Alone",Race)) %>%
  mutate(Race = gsub("American Indian","American Indian or Alaska Native Alone",Race)) %>%
  mutate(Race = gsub("Asian","Asian Alone",Race)) %>%
  mutate(Race = gsub("Caucasian","White Alone",Race)) %>%
  mutate(Race = gsub("Latino/Hispanic of any race","Hispanic or Latino",Race)) %>%
  mutate(Race = gsub("Pacific Islander","Native Hawaiian or Other Pacific Islander Alone",Race)) %>%
  mutate(Race = gsub("Two or More Races","Two or More Race Groups",Race)) %>%
  filter(Race!="Unknown")
  
# Pre-Pandemic Workforce by Race
non.hispanic.workers <- as_tibble(read.xlsx(nonhispanic.url, sheet = "Data", detectDates = TRUE, skipEmptyRows = TRUE, startRow = 1, colNames = TRUE))

non.hispanic.workers <- non.hispanic.workers %>%
  rename(Quarter=X1) %>%
  select(-contains("Flags")) %>%
  pivot_longer(cols = -contains("Quarter"), names_to="Race", values_to="Estimate") %>%
  mutate(Quarter = gsub(" ",":",Quarter), Race = gsub("\\."," ",Race)) %>%
  mutate(Quarter = yq(Quarter))

hispanic.workers <- as_tibble(read.xlsx(hispanic.url, sheet = "Data", detectDates = TRUE, skipEmptyRows = TRUE, startRow = 1, colNames = TRUE))

hispanic.workers <- hispanic.workers %>%
  rename(Quarter=X1) %>%
  select(Quarter, Hispanic.or.Latino) %>%
  pivot_longer(cols = -contains("Quarter"), names_to="Race", values_to="Estimate") %>%
  mutate(Quarter = gsub(" ",":",Quarter), Race = gsub("\\."," ",Race)) %>%
  mutate(Quarter = yq(Quarter))

workforce.by.race <- bind_rows(list(non.hispanic.workers,hispanic.workers))

total.workers <- workforce.by.race %>%
  select(-Race) %>%
  group_by(Quarter) %>%
  summarize_all(sum) %>%
  mutate(Race="Total")

workforce.by.race <- bind_rows(list(workforce.by.race, total.workers))

workforce.by.race <- workforce.by.race %>%
  filter(Quarter=="2020-07-01") %>%
  select(-Quarter) %>%
  rename(WorkForce=Estimate)

region.continuing.claims <- left_join(region.continuing.claims, workforce.by.race, by=c("Race")) %>%
  mutate(Share = (Estimate/WorkForce)*100) %>%
  filter(Race!="Total")

continuing.claims.race.chart <- create_bar_chart(w.data = region.continuing.claims, w.x = "Date", w.y = "Share", w.bartype = "stack", w.transparent = 1.0, w.color = "Race", w.month.breaks= "3 months", w.date.format = "%m/%Y", w.dec=1, w.suff="%")

# Department of Revenue Retail Sales --------------------------------------
sales.tax <- as_tibble(read.xlsx(sales.url, sheet = "TaxableRetailSales", detectDates = TRUE, skipEmptyRows = TRUE, startRow = 10, colNames = TRUE))

sales.tax <- sales.tax %>% mutate(Geography=Year) %>%
  mutate(Geography = str_replace(Geography, "Quarter 1", "")) %>%
  mutate(Geography = str_replace(Geography, "Quarter 2", "")) %>%
  mutate(Geography = str_replace(Geography, "Quarter 3", "")) %>%
  mutate(Geography = str_replace(Geography, "Quarter 4", "")) %>%
  mutate(Geography = str_replace(Geography, "2015", "")) %>%
  mutate(Geography = str_replace(Geography, "2016", "")) %>%
  mutate(Geography = str_replace(Geography, "2017", "")) %>%
  mutate(Geography = str_replace(Geography, "2018", "")) %>%
  mutate(Geography = str_replace(Geography, "2019", "")) %>%
  mutate(Geography = str_replace(Geography, "2020", "")) %>%
  mutate_all(na_if," ") %>%
  fill(Geography) %>%
  mutate(Year = str_replace(Year, " Quarter ", ":Q")) %>%
  rename(Quarter=Year) %>%
  select(-NAICS,-Units)

sales.tax <- na.omit(sales.tax)
sales.tax$Quarter <- yq(sales.tax$Quarter)

regional.sales.tax <- sales.tax %>%
  select(-Geography) %>%
  group_by(Quarter, Tax.Type) %>%
  summarize_all(sum)

region.salestax.chart <- create_bar_chart(w.data = regional.sales.tax, w.x = "Quarter", w.y = "Total.Taxable", w.bartype = "stack", w.transparent = 1.0, w.color = "Tax.Type", w.month.breaks= "1 year", w.date.format = "%b-%Y")

tax.19 <- sales.tax %>% filter(Quarter =="2019-10-01", Tax.Type == "Sales") %>% rename(Q42019=Total.Taxable) %>% select("Geography","Q42019")
tax.20 <- sales.tax %>% filter(Quarter =="2020-10-01", Tax.Type == "Sales") %>% rename(Q42020=Total.Taxable) %>% select("Geography","Q42020")

tax.comparison <- left_join(tax.19, tax.20, by=c("Geography")) %>%
  mutate(Geography = str_replace(Geography, "/king", "")) %>%
  mutate(Geography = str_replace(Geography, "/pierce", "")) %>%
  mutate(Geography = str_replace(Geography, "/snohomish", "")) %>%
  group_by(Geography) %>%
  summarize_all(sum) %>%
  mutate(Geography = str_replace(Geography, " City", "")) %>%
  mutate(Geography = str_replace(Geography, "Beaux Arts Village", "Beaux Arts")) %>%
  mutate(Geography = str_replace(Geography, "Du Pont", "DuPont")) %>%
  mutate(Geography = str_replace(Geography, "Seatac", "Sea Tac")) %>%
  mutate(Ratio = Q42020/Q42019)

tax.comparison <- left_join(tax.comparison, regeo, by=c("Geography"= "city_name")) %>% mutate(class_desc=replace_na(class_desc,"Unincorporated County"))
tax.comparison$class_desc <- factor(tax.comparison$class_desc, levels=city.class.order)

# Create Plot
city.salestax.chart <-  ggplotly(ggplot(data = tax.comparison,
                                        aes(x = reorder(Geography,-Ratio), 
                                            y = Ratio, 
                                            fill = Geography,
                                            text = paste0("<b>", Geography, ": </b>", prettyNum(round(Ratio, 2), big.mark = ","), "<br>"))) +
                                   geom_col(color = "black",
                                            alpha = 1.0,
                                            position = "dodge") +
                                   geom_hline(yintercept=1.0,
                                              size = 1.0,
                                              color = "black") +
                                   labs(x = NULL, y = NULL) +
                                   scale_y_continuous(labels = label_comma()) +
                                   theme(plot.title = element_text(size = 10, face = 'bold'),
                                         axis.text.x = element_blank(),
                                         axis.ticks.x = element_blank(),
                                         axis.line = element_blank(),
                                         panel.background = element_blank(),
                                         panel.grid.major.y = element_line(colour="#BBBDC0",size = 0.25),
                                         panel.grid.minor.y = element_line(colour="#BBBDC0",size = 0.25),
                                         panel.grid.major.x = element_blank(),
                                         panel.grid.minor.x = element_blank(),
                                         text = element_text(family = "Segoe UI"),
                                         legend.position = "none",
                                         legend.title = element_blank())+
                                   facet_wrap(vars(class_desc), scales = "free", ncol=3) +
                                   theme(panel.spacing.y = unit(4, "lines")),
                                 tooltip = c("text"))
  

# Home Sales --------------------------------------
home.sales <- as_tibble(read.xlsx(homesales.url, sheet = "Sheet 1", detectDates = TRUE, skipEmptyRows = TRUE, startRow = 1, colNames = TRUE))

median.price <- home.sales %>% select(Month.of.Period.End, Median.Sale.Price) %>% mutate(Month.of.Period.End = my(Month.of.Period.End)) %>% rename(Month=Month.of.Period.End) %>% mutate(Variable="Median Sales Price") 
homes.sold <- home.sales %>% select(Month.of.Period.End, Homes.Sold) %>% mutate(Month.of.Period.End = my(Month.of.Period.End)) %>% rename(Month=Month.of.Period.End) %>% mutate(Variable="Homes Sold") 
homes.inventory <- home.sales %>% select(Month.of.Period.End, Inventory) %>% mutate(Month.of.Period.End = my(Month.of.Period.End)) %>% rename(Month=Month.of.Period.End) %>% mutate(Variable="Home Inventory") 

region.mediansales.chart <- create_bar_chart(w.data = median.price, w.x = "Month", w.y = "Median.Sale.Price", w.bartype = "dodge", w.transparent = 1.0, w.color = "Variable", w.month.breaks= "1 year", w.date.format = "%b-%Y")
region.homesales.chart <- create_bar_chart(w.data = homes.sold, w.x = "Month", w.y = "Homes.Sold", w.bartype = "dodge", w.transparent = 1.0, w.color = "Variable", w.month.breaks= "2 years", w.date.format = "%b-%Y")
region.homeinventory.chart <- create_bar_chart(w.data = homes.inventory, w.x = "Month", w.y = "Inventory", w.bartype = "dodge", w.transparent = 1.0, w.color = "Variable", w.month.breaks= "2 years", w.date.format = "%b-%Y")

# Rentals --------------------------------------
rentals <- as_tibble(read.csv(rentals.url))

region.rentals <- rentals %>% 
  filter(RegionName == "Seattle, WA") %>% 
  select(-RegionID, -SizeRank) %>% 
  pivot_longer(cols=contains("X"), names_to="Year", values_to="Estimate") %>%
  mutate(Year = str_replace(Year, "X", "")) %>%
  mutate(Year = ym(Year))

region.rentals.chart <- create_bar_chart(w.data = region.rentals, w.x = "Year", w.y = "Estimate", w.bartype = "dodge", w.transparent = 1.0, w.color = "RegionName", w.month.breaks= "1 year", w.date.format = "%b-%Y")

# Transit Data ------------------------------------------------------------
ntd.data <- as_tibble(read.xlsx(ntd.url, sheet = "UPT", detectDates = TRUE, skipEmptyRows = TRUE, startRow = 1, colNames = TRUE))

ntd.psrc <- ntd.data %>%
  filter(`4.digit.NTD.ID` %in% psrc.transit, Modes!="DR") %>%
  mutate(Agency = gsub("Snohomish County Public Transportation Benefit Area Corporation","Community Transit",Agency)) %>%
  mutate(Agency = gsub("Central Puget Sound Regional Transit Authority","Sound Transit",Agency)) %>%
  mutate(Agency = gsub("King County Department of Metro Transit","King County Metro",Agency)) %>%
  mutate(Agency = gsub("Pierce County Transportation Benefit Area Authority","Pierce Transit",Agency)) %>%
  mutate(Agency = gsub("City of Everett","Everett Transit",Agency)) %>%
  mutate(Agency = gsub("King County Ferry District","King County Metro",Agency)) %>%
  select(-`4.digit.NTD.ID`, -`5.digit.NTD.ID`, -Active, -Reporter.Type, -UZA, -UZA.Name, -TOS) %>%
  rename(AgencyModes=Modes) %>%
  pivot_longer(cols = -contains("Agency"), names_to="Month", values_to="Estimate") %>%
  mutate(Month=my(Month)) %>%
  na.omit(Estimate) %>%
  rename(Modes=AgencyModes)

ntd.start.year <- 2010
ntd.end.year <- max(year(ntd.psrc$Month))
ntd.end.month <- ntd.psrc %>% select(Month) %>% filter(year(Month) == ntd.end.year) %>% pull() %>% max() %>% month()

# Year to Date Transit by Mode  
crt.by.agency <- ntd.psrc %>% 
  filter(Modes %in% commuter.rail.modes & year(Month) >= ntd.start.year & month(Month) <= ntd.end.month) %>%
  mutate(Year = year(Month)) %>%
  select(Agency, Year, Estimate) %>%
  group_by(Agency,Year) %>%
  summarize_all(sum) %>%
  mutate(Mode="Commuter Rail")

lrt.by.agency <- ntd.psrc %>% 
  filter(Modes %in% light.rail.modes & year(Month) >= ntd.start.year & month(Month) <= ntd.end.month) %>%
  mutate(Year = year(Month)) %>%
  select(Agency, Year, Estimate) %>%
  group_by(Agency,Year) %>%
  summarize_all(sum) %>%
  mutate(Mode="Light Rail, Streetcar & Monorail")

fry.by.agency <- ntd.psrc %>% 
  filter(Modes %in% ferry.modes & year(Month) >= ntd.start.year & month(Month) <= ntd.end.month) %>%
  mutate(Year = year(Month)) %>%
  select(Agency, Year, Estimate) %>%
  group_by(Agency,Year) %>%
  summarize_all(sum) %>%
  mutate(Mode="Ferry")

bus.by.agency <- ntd.psrc %>% 
  filter(Modes %in% bus.modes & year(Month) >= ntd.start.year & month(Month) <= ntd.end.month) %>%
  mutate(Year = year(Month)) %>%
  select(Agency, Year, Estimate) %>%
  group_by(Agency,Year) %>%
  summarize_all(sum) %>%
  mutate(Mode="Bus")

transit.by.agency <- bind_rows(list(crt.by.agency, lrt.by.agency, fry.by.agency, bus.by.agency))

transit.by.agency$Agency <- factor(transit.by.agency$Agency, levels=transit.agency.order)

# Create Facet Plot
transit.by.agency.chart <-  ggplotly(ggplot(data = transit.by.agency,
                                        aes(x = Year, 
                                            y = Estimate, 
                                            fill = Agency,
                                            text = paste0("<b>", Agency, ": </b>", prettyNum(round(Estimate, -1), big.mark = ","), "<br>"))) +
                                   geom_col(color = "black",
                                            alpha = 1.0,
                                            position = "stack") +
                                   labs(x= NULL, y = NULL) +
                                   scale_y_continuous(labels = label_comma()) +
                                   scale_fill_manual(values = psrc_colors) +
                                   theme(plot.title = element_text(size = 10, face = 'bold'),
                                         axis.text.x = element_text(size = 10, face = 'bold'),
                                         axis.ticks.x = element_blank(),
                                         axis.line = element_blank(),
                                         panel.background = element_blank(),
                                         panel.grid.major.y = element_line(colour="#BBBDC0",size = 0.25),
                                         panel.grid.minor.y = element_line(colour="#BBBDC0",size = 0.25),
                                         panel.grid.major.x = element_blank(),
                                         panel.grid.minor.x = element_blank(),
                                         text = element_text(family = "Segoe UI"),
                                         legend.position = "none",
                                         legend.title = element_blank())+
                                   facet_wrap(vars(Mode), scales = "free", ncol=2) +
                                   theme(panel.spacing.y = unit(4, "lines")),
                                 tooltip = c("text"))

transit2019 <- transit.by.agency %>% filter(Year==2019) %>% select(Estimate) %>% pull(Estimate) %>% sum()
transit2020 <- transit.by.agency %>% filter(Year==2020) %>% select(Estimate) %>% pull(Estimate) %>% sum()
transit2021 <- transit.by.agency %>% filter(Year==2021) %>% select(Estimate) %>% pull(Estimate) %>% sum()

ratio2021 <- transit2021 / transit2019

# Volume Data -------------------------------------------------------------
volume_data <- setDT(read.csv(volume_file,stringsAsFactors=FALSE))
nms <- c("Highway","County","Location","Date","Previous_Date","Measure","Day_of_Week","Current_Volume","Baseline_Volume")
setnames(volume_data,nms)

traffic.volumes <- as_tibble(volume_data) %>%
  filter(County %in% c("King","Kitsap","Pierce","Snohomish"), Measure == "Current") %>%
  mutate(Ratio = Current_Volume / Baseline_Volume) %>%
  mutate(Date = str_sub(Date,-8), Date = mdy(Date)) %>%
  select(-Previous_Date, -Measure, -Day_of_Week) %>%
  filter(year(Date) == 2021)

all.highways.19 <- traffic.volumes %>% select(Baseline_Volume) %>% pull() %>% sum()
all.highways.21 <- traffic.volumes %>% select(Current_Volume) %>% pull() %>% sum()
traffic.share.21 <- all.highways.21 / all.highways.19

i5.i405.19 <- traffic.volumes %>% filter(str_detect(Location, 'I-5')|str_detect(Location, 'I-405')) %>% select(Baseline_Volume) %>% pull() %>% sum()
i5.i405.21 <- traffic.volumes %>% filter(str_detect(Location, 'I-5')|str_detect(Location, 'I-405')) %>% select(Current_Volume) %>% pull() %>% sum()
i5.i405.share.21 <- i5.i405.21 / i5.i405.19

crosslake.19 <- traffic.volumes %>% filter(str_detect(Location, 'I-90')|str_detect(Location, 'SR 520')) %>% select(Baseline_Volume) %>% pull() %>% sum()
crosslake.21 <- traffic.volumes %>% filter(str_detect(Location, 'I-90')|str_detect(Location, 'SR 520')) %>% select(Current_Volume) %>% pull() %>% sum()
crosslake.share.21 <- crosslake.21 / crosslake.19

other.19 <- traffic.volumes %>% filter(str_detect(Location, 'SR 167')|str_detect(Location, 'SR 410')|str_detect(Location, 'SR 512')) %>% select(Baseline_Volume) %>% pull() %>% sum()
other.21 <- traffic.volumes %>% filter(str_detect(Location, 'SR 167')|str_detect(Location, 'SR 410')|str_detect(Location, 'SR 512')) %>% select(Current_Volume) %>% pull() %>% sum()
other.share.21 <- other.21 / other.19

i5.traffic.volumes.chart <- create_line_chart(w_tbl=traffic.volumes %>% filter(str_detect(Location, 'I-5')) %>% select(Location, Date, Ratio), w_title="Share of 2019 Volume", w_label=label_percent(), w_dec=0, w_group="Location", w_factor=100, w_suff="%")
i405.traffic.volumes.chart <- create_line_chart(w_tbl=traffic.volumes %>% filter(str_detect(Location, 'I-405')) %>% select(Location, Date, Ratio), w_title="Share of 2019 Volume", w_label=label_percent(), w_dec=0, w_group="Location", w_factor=100, w_suff="%")
crosslake.traffic.volumes.chart <- create_line_chart(w_tbl=traffic.volumes %>% filter(str_detect(Location, 'I-90')|str_detect(Location, 'SR 520')) %>% select(Location, Date, Ratio), w_title="Share of 2019 Volume", w_label=label_percent(), w_dec=0, w_group="Location", w_factor=100, w_suff="%")
sr.traffic.volumes.chart <- create_line_chart(w_tbl=traffic.volumes %>% filter(str_detect(Location, 'SR 167')|str_detect(Location, 'SR 410')|str_detect(Location, 'SR 512')) %>% select(Location, Date, Ratio), w_title="Share of 2019 Volume", w_label=label_percent(), w_dec=0, w_group="Location", w_factor=100, w_suff="%")

# Truck Data -------------------------------------------------------------
truck_data <- setDT(read.csv(truck_file,stringsAsFactors=FALSE))
nms <- c("Highway","County","Location","Date","Previous_Date","Note","Day_of_Week","Ratio")
setnames(truck_data,nms)

truck.volumes <- as_tibble(truck_data) %>%
  filter(County %in% c("King","Kitsap","Pierce","Snohomish")) %>%
  mutate(Date = str_sub(Date,-8), Date = mdy(Date)) %>%
  select(-Previous_Date, -Note, -Day_of_Week) %>%
  filter(year(Date) == 2021) %>%
  mutate(Ratio=gsub("%","",Ratio), Ratio=as.numeric(Ratio), Ratio=(Ratio/100)+1) %>%
  na.omit(Ratio)

i5.truck.volumes.chart <- create_line_chart(w_tbl=truck.volumes %>% filter(str_detect(Location, 'I-5')) %>% select(Location, Date, Ratio), w_title="Share of 2019 Volume", w_label=label_percent(), w_dec=0, w_group="Location", w_factor=100, w_suff="%")
i405.truck.volumes.chart <- create_line_chart(w_tbl=truck.volumes %>% filter(str_detect(Location, 'I-405')) %>% select(Location, Date, Ratio), w_title="Share of 2019 Volume", w_label=label_percent(), w_dec=0, w_group="Location", w_factor=100, w_suff="%")
crosslake.truck.volumes.chart <- create_line_chart(w_tbl=truck.volumes %>% filter(str_detect(Location, 'I-90')|str_detect(Location, 'SR 520')) %>% select(Location, Date, Ratio), w_title="Share of 2019 Volume", w_label=label_percent(), w_dec=0, w_group="Location", w_factor=100, w_suff="%")
sr.truck.volumes.chart <- create_line_chart(w_tbl=truck.volumes %>% filter(str_detect(Location, 'SR 16')|str_detect(Location, 'SR 167')|str_detect(Location, 'SR 410')|str_detect(Location, 'SR 512')) %>% select(Location, Date, Ratio), w_title="Share of 2019 Volume", w_label=label_percent(), w_dec=0, w_group="Location", w_factor=100, w_suff="%")

# Ferry Data -------------------------------------------------------------
ferry_data <- setDT(read.csv(ferry_file,stringsAsFactors=FALSE))
nms <- c("Date","Previous_Date","Route","Measure","Old","Ratio")
setnames(ferry_data,nms)

ferry.volumes <- as_tibble(ferry_data) %>%
  mutate(Date = mdy(Date)) %>%
  select(-Previous_Date, -Old) %>%
  filter(year(Date) == 2021) %>%
  filter(Route %in% c("Edmonds -  Kingston", "Fauntleroy - Vashon - Southworth", "Mukilteo - Clinton", "Point Defiance - Tahlequah" , "Port Townsend - Coupeville", "Seattle - Bainbridge Island", "Seattle - Bremerton"))

ferry.19 <- ferry.volumes %>% filter(Measure=="Baseline year  Ridership") %>% select("Ratio") %>% pull() %>% sum()
ferry.21 <- ferry.volumes %>% filter(Measure=="Current  Ridership") %>% select("Ratio") %>% pull() %>% sum()
ferry.ratio <- ferry.21 / ferry.19

ferry.volumes.chart <- create_line_chart(w_tbl=ferry.volumes %>% filter(Measure=="Percentage Change") %>% select(Route, Date, Ratio) %>% mutate(Ratio=Ratio+1), w_title="Share of 2019", w_label=label_percent(), w_dec=0, w_group="Route", w_factor=100, w_suff="%")

