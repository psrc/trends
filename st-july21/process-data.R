# Packages ----------------------------------------------------------------

# Data Cleaning/Processing
library(tidyr)
library(dplyr)
library(stringr)
library(openxlsx)
library(lubridate)
library(pdftools)
library(data.table)
library(readxl)

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
sales.url <- "X:/DSA/shiny-uploads/trends/st-jul21/data/TaxableRetailSalesJuly21.xlsx"
volume_file <- "X:/DSA/shiny-uploads/trends/st-jul21/data/VolumeNumTableCountLocation_data.csv"
psef.forcast <- "X:/DSA/shiny-uploads/trends/st-jul21/data/Quarterly-Forecast-0321.xls"

# Variables and Lists -----------------------------------------------------
starting.year <- 2010

psrc_counties <- c("King","Kitsap","Pierce","Snohomish")

i5.north.counts <- c("I-5 at Northgate", "I-5 at King/Snohomish county line", "I-5 at Lynnwood (156th)", "I-5 at Everett (Marine View Drive)")

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
  "City of Seattle" = "#F7A489",
  "COVID Recession" = "#F05A28",
  "Dot-Com Recession" = "#8CC63E",
  "Great Recession" = "#00A7A0"
  
)

city.class.order <- c("Metro","Core","HCT","CitiesTowns","Unincorporated County")

# Inputs for PSEF Forecast
pre.covid.peak <- yq("2020:Q1")
current.quarter <- yq("2020:Q4")
pre.gr.peak <- yq("2008:Q1")
pre.dc.peak <- yq("2000:Q4")
pre.dc.end <- yq("2005:Q4")


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

# Combine components of change into one tibble
ofm.population.change <- bind_rows(list(ofm.pop.chg.natural, ofm.pop.chg.migration))

# Region Summary
region.pop <- ofm.population.change %>% filter(Jurisdiction=="Region Total", Variable !="Total Change", Year >=starting.year) %>% select(Jurisdiction, Year, Estimate, Variable)
region.pop$Year <- as.Date(paste(region.pop$Year, 4, 1, sep = "-"))
region.pop.change.chart <- create_bar_chart(w.data = region.pop, w.x = "Year", w.y = "Estimate", w.bartype = "stack", w.transparent = 1.0, w.color = "Variable", w.month.breaks= "1 year", w.date.format = "%Y")

rm(ofm.pop.chg.migration, ofm.pop.chg.natural,ofm.population.change, city.lyr)
file.remove(ofm.pop.chg.file)

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

region.jobs <- msa.jobs %>% filter(NAICS.INDUSTRY == "Total Nonfarm", year(Month) >= starting.year, month(Month) == latest.month, Geography=="Region") %>% mutate(Delta = Estimate - lag(Estimate, order_by = Month)) %>% mutate(Year = year(Month))
region.job.total.chart <- create_bar_chart(w.data = region.jobs, w.x = "Month", w.y = "Estimate", w.bartype = "dodge", w.transparent = 1.0, w.color = "NAICS.INDUSTRY", w.month.breaks= "1 year", w.date.format = "%Y")

# Delete the temporary downloaded file
file.remove(esd.file)
rm(working, msa.jobs)

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
  
rm(sales.tax, tax.19, tax.20)

# Volume Data -------------------------------------------------------------
volume_data <- setDT(read.csv(volume_file,stringsAsFactors=FALSE))
nms <- c("Highway","County","Location","Date","Previous_Date","Measure","Day_of_Week","Current_Volume","Baseline_Volume")
setnames(volume_data,nms)

traffic.volumes <- as_tibble(volume_data) %>%
  filter(County %in% c("King","Kitsap","Pierce","Snohomish"), Measure == "Current") %>%
  mutate(Ratio = Current_Volume / Baseline_Volume) %>%
  mutate(Date = str_sub(Date,-8), Date = mdy(Date)) %>%
  select(-Previous_Date, -Measure, -Day_of_Week) %>%
  filter(year(Date) == 2021) %>%
  filter(Location %in% i5.north.counts)

i5.19 <- traffic.volumes %>% filter(str_detect(Location, 'I-5')|str_detect(Location, 'I-405')) %>% select(Baseline_Volume) %>% pull() %>% sum()
i5.21 <- traffic.volumes %>% filter(str_detect(Location, 'I-5')|str_detect(Location, 'I-405')) %>% select(Current_Volume) %>% pull() %>% sum()
i5.share.21 <- i5.21 / i5.19

i5.traffic.volumes.chart <- create_line_chart(w_tbl=traffic.volumes %>% filter(str_detect(Location, 'I-5')) %>% select(Location, Date, Ratio), w_title="Share of 2019 Volume", w_label=label_percent(), w_dec=0, w_group="Location", w_factor=100, w_suff="%")

rm(volume_data)

# PSEF Forecast -----------------------------------------------------------
col.thousands <- c("Employment (thous.)", "Goods producing", "Natural resources", "Construction", "Manufacturing", "Aerospace",
                   "Other durable goods", "Nondurable goods", "Services producing", "Wholesale and retail trade", "Transportation and public utilities",
                   "Information", "Financial activities", "Professional and business services", "Other services", "Government", "State and local", "Federal",
                   "Housing permits (thous.)", "Population (thous.)", "Net migration (thous.)")

col.shares <- c("Unemployment rate (%)", "Employment", "Personal income (cur. $)", "Consumer price index", "Housing permits", "Population")

col.billions <- c("Personal income (bils. $12)","Personal income (bils. $)","Wage and salary disbursements", "Other income")

col.asis <- c("Per capita personal income ($)", "Consumer price index (82-84=1.000)")

region.forecast.data  <- read_excel(psef.forcast, sheet="Region", skip=9) %>%
  drop_na() %>%
  rename(Metric=1) %>%
  select(-2) %>%
  pivot_longer(!Metric, names_to = "quarter") %>%
  separate(quarter, c("Year","Quarter"), "\\.") %>%
  mutate(Quarter = gsub("0999999999999", "1", Quarter), Quarter = gsub("4000000000001", "4", Quarter)) %>%
  mutate(across(c('Year','Quarter'), as.numeric)) %>%
  mutate(Estimate = case_when(
    Metric %in% col.thousands ~ value*1000,
    Metric %in% col.shares ~ value/100,
    Metric %in% col.billions ~ value*1000000000,
    Metric %in% col.asis ~ value)) %>%
  select(-value)

pre.covid.jobs <- region.forecast.data %>% filter(Metric == "Employment (thous.)") %>% mutate(DataDate = yq(paste0(Year,":Q",Quarter))) %>% 
  filter(DataDate==pre.covid.peak) %>% select(Estimate) %>% pull() %>% round(-1)

current.jobs <- region.forecast.data %>% filter(Metric == "Employment (thous.)") %>% mutate(DataDate = yq(paste0(Year,":Q",Quarter))) %>% 
  filter(DataDate==current.quarter) %>% select(Estimate) %>% pull() %>% round(-1)

covid.return.to.pk.jobs <- region.forecast.data %>% filter(Metric == "Employment (thous.)") %>% mutate(DataDate = yq(paste0(Year,":Q",Quarter))) %>%
  filter(DataDate>=current.quarter & Estimate >= pre.covid.jobs) %>% slice(1:1) %>% select(DataDate) %>% pull()

covid.yrs.to.recover <- round(int_length(interval(pre.covid.peak, covid.return.to.pk.jobs)) / 31536000,1)

covid.job.data <- region.forecast.data %>% 
  mutate(DataDate = yq(paste0(Year,":Q",Quarter))) %>%
  select(DataDate, Metric, Estimate) %>%
  filter(DataDate >= pre.covid.peak & Metric == "Employment (thous.)" & Estimate <= pre.covid.jobs) %>%
  mutate(Quarter = row_number(), Recession="COVID Recession")

pre.gr.jobs <- region.forecast.data %>% filter(Metric == "Employment (thous.)") %>% mutate(DataDate = yq(paste0(Year,":Q",Quarter))) %>% 
  filter(DataDate==pre.gr.peak) %>% select(Estimate) %>% pull() %>% round(-1)

gr.return.to.pk.jobs <- region.forecast.data %>% filter(Metric == "Employment (thous.)") %>% mutate(DataDate = yq(paste0(Year,":Q",Quarter))) %>%
  filter(DataDate>=pre.gr.peak & Estimate >= pre.gr.jobs) %>% slice(1:1) %>% select(DataDate) %>% pull()

gr.yrs.to.recover <- round(int_length(interval(pre.gr.peak, gr.return.to.pk.jobs)) / 31536000,1)

gr.job.data <- region.forecast.data %>% 
  mutate(DataDate = yq(paste0(Year,":Q",Quarter))) %>%
  select(DataDate, Metric, Estimate) %>%
  filter(DataDate > pre.gr.peak & Metric == "Employment (thous.)" & Estimate <= pre.gr.jobs) %>%
  mutate(Quarter = row_number(), Recession="Great Recession")

pre.dc.jobs <- region.forecast.data %>% filter(Metric == "Employment (thous.)") %>% mutate(DataDate = yq(paste0(Year,":Q",Quarter))) %>% 
  filter(DataDate==pre.dc.peak) %>% select(Estimate) %>% pull() %>% round(-1)

dc.return.to.pk.jobs <- region.forecast.data %>% filter(Metric == "Employment (thous.)") %>% mutate(DataDate = yq(paste0(Year,":Q",Quarter))) %>%
  filter(DataDate>=pre.dc.peak & Estimate >= pre.dc.jobs) %>% slice(1:1) %>% select(DataDate) %>% pull()

dc.yrs.to.recover <- round(int_length(interval(pre.dc.peak, dc.return.to.pk.jobs)) / 31536000,1)

dc.job.data <- region.forecast.data %>% 
  mutate(DataDate = yq(paste0(Year,":Q",Quarter))) %>%
  select(DataDate, Metric, Estimate) %>%
  filter(DataDate > pre.dc.peak & Metric == "Employment (thous.)" & DataDate <= pre.dc.end) %>%
  mutate(Quarter = row_number(), Recession="Dot-Com Recession")

recession.data <- bind_rows(list(covid.job.data, gr.job.data, dc.job.data))

recession.chart <- ggplotly(ggplot(data=recession.data, 
                                   aes(y=Estimate, 
                                       x=Quarter, 
                                       group=Recession, 
                                       color=factor(Recession),
                                       text = paste0(month(`DataDate`),"-",day(`DataDate`),"-",year(`DataDate`)," Jobs: ",prettyNum(round(Estimate, -1), big.mark = ","))))+
                                         geom_line(size=1.2) + 
                                         scale_color_manual(values=psrc_colors)+
                                         scale_y_continuous(labels = label_comma()) +
                                         ylab("Total Jobs")+
                                         xlab("Quarter") +
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
