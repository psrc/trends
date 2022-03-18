library(data.table)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(plotly)
library(openxlsx)
library(sf)
library(here)

# Inputs ------------------------------------------------------------------
srv.dir <- 'X:/DSA/shiny-uploads/trends/rsc-mar22'

cont.claims.file <- file.path(srv.dir,'data','Continued Claims Demographics Published.37.xlsx')
nonhispanic.file <- file.path(srv.dir,'data','workers-by-race-quarter.xlsx')
hispanic.file <- file.path(srv.dir,'data','workers-by-hispanic-origin-race.xlsx')
gender.file <- file.path(srv.dir,'data','workers-by-gender.xlsx')
age.file <- file.path(srv.dir,'data','workers-by-age.xlsx')
edu.file <-file.path(srv.dir,'data','workers-by-education.xlsx')
init.claims.file <- file.path(srv.dir,'data', 'Initial Claims Demographics Week 9, 2020 to Week 37, 2021 Published.xlsx')

rentals.file <- file.path(srv.dir,'data', 'Metro_ZORI_AllHomesPlusMultifamily_Smoothed.csv')
home.sales.file <- file.path(srv.dir,'data', 'HomeSalesRedfin.xlsx')

transit.file <- file.path(srv.dir,'data', 'January-2022-Raw-Database.xlsx')

ofm.pop.file <- file.path(srv.dir,'data', 'ofm_april1_population_final.xlsx')
ofm.pop.chg.file <- file.path(srv.dir,'data', 'ofm_april1_components_of_change_1960_to_present.xlsx')
ofm.housing.file <- file.path(srv.dir,'data', 'ofm_april1_postcensal_estimates_housing_1980_1990-present.xlsx')

sales.file <- file.path(srv.dir,'data', 'TaxableRetailSales-Local.xlsx')

education.lookup <- c("Did not finish high school" = "Less than high school",
                "High School Diploma, including GED" = "High School",
                "Associate's Degree" = "Some College",
                "Some College" = "Some College",
                "Bachelor's Degree" = "College degree",
                "Master's Degree" = "College degree",
                "PhD" = "College degree",
                "Post-Baccalaureate Degree" = "College degree",
                "No Schooling" = "Under age 24",
                "Unknown" = "Under age 24",
                "Less than high school" = "Less than high school",
                "High school or equivalent, no college"= "High School",
                "Some college or Associate degree" = "Some College",
                "Bachelor's degree or advanced degree" = "College degree",
                "Educational attainment not available (workers aged 24 or younger)" = "Under age 24",
                "Total" = "Total")

education.lookup <- enframe(education.lookup)

psrc.counties <- c("King","Kitsap","Pierce","Snohomish")

psrc.colors <- c("2015" = "#91268F",
                 "2018" = "#00A7A0",
                 "2019" = "#91268F",
                 "2020" = "#F05A28",
                 "2021" = "#8CC63E",
                 "2022" = "#00A7A0",
                 "Moderate" = "orange",
                 "Heavy" = "red",
                 "Severe" = "black",
                 "Region" = "#91268F",
                 "Washington State" = "#4C4C4C",
                 "Statewide" = "#4C4C4C",
                 "Construction" = "#8CC63E",
                 "Mining and Logging" = "#A9D46E",
                 "Manufacturing" = "#588527",
                 "Financial Activities" = "#630460",
                 "Information" = "#91268F",
                 "Professional and Business Services" = "#AD5CAB",
                 "Leisure and Hospitality" = "#9f3913",
                 "Trade, Transportation, and Utilities" = "#F05A28",
                 "Other Services" = "#F4835E",
                 "Government" = "#00716c", 
                 "Educational and Health Services" = "#00A7A0",
                 "Total Nonfarm" = "#40BDB8",
                 "Black or African American Alone" = "#91268F",
                 "American Indian or Alaska Native Alone" = "#8CC63E",
                 "Asian Alone" = "#00A7A0",
                 "White Alone" = "#76787A",
                 "Hispanic or Latino" = "#F05A28",
                 "Native Hawaiian or Other Pacific Islander Alone" = "#630460",
                 "Two or More Race Groups" = "#C0E095",
                 "Male" = "#00A7A0",
                 "Female" = "#8CC63E",
                 "King County" = "#C388C2",
                 "Kitsap County" = "#F05A28",
                 "Pierce County" = "#8CC63E",
                 "Snohomish County" = "#00A7A0",
                 "Migration" = "#8CC63E",
                 "Natural Increase" = "#F05A28",
                 "Single-Family" = "#91268F",
                 "Multi-Family" = "#00A7A0",
                 "Mobile-Home" = "#F05A28",
                 "Seattle, WA" = "#8CC63E",
                 "Median Sales Price" = "#F05A28",
                 "Homes Sold" = "#00A7A0",
                 "Home Inventory" = "#91268F",
                 "18-24" = "#4a0048",
                 "25-34" = "#630460",
                 "35-44" = "#91268F",
                 "45-54" = "#AD5CAB",
                 "55-64" = "#C388C2",
                 "65+" = "#E3C9E3",
                 "College degree" = "#9f3913",
                 "Under age 24" = "#F05A28",
                 "High School" = "#F4835E",
                 "Less than high school" = "#F7A489",
                 "Some College" = "#FBD6C9",
                 "I-405 at Tukwila (Longacres)" = "#005753",
                 "I-405 at Renton (37th)" = "#00716c",
                 "I-405 at Kirkland (NE 132nd Street)" = "#00A7A0",
                 "I-405 at Bellevue (NE 8th Street)" = "#40BDB8",
                 "I-405 at Bothell (Filbert)" = "#73CFCB",
                 "I-5 at SODO (Holden)" = "#4a0048",
                 "I-5 at Seattle/SLU (Boylston)" = "#630460",
                 "I-5 at Northgate" = "#91268F",
                 "I-5 at King/Snohomish county line" = "#AD5CAB",
                 "I-5 at Federal Way (Enchanted Pkwy)" = "#C388C2",
                 "I-5 at Pierce/King county line" = "#E3C9E3",
                 "I-5 at Lakewood" = "#F4835E",
                 "I-5 at Lynnwood (156th)" = "#F7A489",
                 "I-5 at Everett (Marine View Drive)" = "#FBD6C9",
                 "SR 167 at Kent (196th)" = "#3f6618",
                 "SR 512 at Pacific Ave. (SR 7)" = "#588527",
                 "SR 16 at Tacoma Narrows" = "#8CC63E",
                 "SR 410 at Sumner" = "#A9D46E",
                 "SR 520 at SR 520 Floating Bridge" = "#C0E095",
                 "I-90 at West of Bandera" = "#E2F1CF",
                 "I-90 at I-90 Floating Bridge" = "#005753",
                 "Sales" = "#8CC63E",
                 "Use" = "#F05A28")

city.of.interest <- c("Bremerton", "Bellevue", "Everett","Seattle",  "Tacoma")

county.of.interest <- c("Kitsap County", "King County", "Pierce County", "Snohomish County")

sectors.of.interest <- c("Manufacturing", "Leisure and Hospitality", "Construction", "Other Services", "Professional and Business Services", "Educational and Health Services")

city.class.order <- c("Metro","Core","HCT","CitiesTowns","Unincorporated County")

# Functions ---------------------------------------------------------------
create.line.chart <- function(t, w.x, w.y, w.yr, w.g, w.title, f=1, d=0, p="", s="", w.lab, w.x.title=NULL, x.type="cont", w.lwidth=1, d.form="%b-%Y", w.breaks=NULL) {
  
  if (x.type=="cont") {
    g <- ggplotly(ggplot(data=t, 
                         aes(x=get(eval(w.x)),
                             y=get(eval(w.y)), 
                             group=get(eval(w.g)),
                             text = paste0("<b>Year: </b>",  get(eval(w.yr)), "<br>","<b>",w.title,": </b>", p, prettyNum(round(get(eval(w.y))*f, d), big.mark = ","), s, "<br>")))  + 
                    geom_line(aes(color=get(eval(w.g))))+
                    geom_point(aes(color=get(eval(w.g))))+
                    scale_x_discrete(breaks=w.breaks) +
                    scale_y_continuous(labels = w.lab) +
                    scale_color_manual(values = psrc.colors) +
                    labs(x=w.x.title, y=w.title) +
                    theme(plot.title = element_text(size = 10, face = 'bold'),
                          axis.ticks.x = element_blank(),
                          axis.line.x = element_line(colour="#BBBDC0",size = 0.25),
                          axis.line.y = element_line(colour="#BBBDC0",size = 0.25),
                          panel.background = element_blank(),
                          panel.grid.major.y = element_line(colour="#BBBDC0",size = 0.25),
                          panel.grid.minor.y = element_line(colour="#BBBDC0",size = 0.25),
                          panel.grid.major.x = element_blank(),
                          panel.grid.minor.x = element_blank(),
                          legend.position = "bottom",
                          legend.title = element_blank()),
                  tooltip = c("text")) %>% layout(legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.25),hovermode = "x")
    
  } else {
    g <- ggplotly(ggplot(data=t, 
                         aes(x=get(eval(w.x)),
                             y=get(eval(w.y)), 
                             group=get(eval(w.g)),
                             text = paste0("<b>Year: </b>",  get(eval(w.yr)), "<br>","<b>",w.title,": </b>", p, prettyNum(round(get(eval(w.y))*f, d), big.mark = ","), s, "<br>")))  + 
                    geom_line(aes(color=get(eval(w.g))), size = w.lwidth)+
                    scale_x_date(labels = date_format(d.form)) +
                    scale_y_continuous(labels = w.lab) +
                    scale_color_manual(values = psrc.colors) +
                    labs(x=w.x.title, y=w.title) +
                    theme(plot.title = element_text(size = 10, face = 'bold'),
                          axis.line.x = element_line(colour="#BBBDC0",size = 0.25),
                          axis.line.y = element_line(colour="#BBBDC0",size = 0.25),
                          panel.background = element_blank(),
                          panel.grid.major.y = element_line(colour="#BBBDC0",size = 0.25),
                          panel.grid.minor.y = element_blank(),
                          panel.grid.major.x = element_line(colour="#BBBDC0",size = 0.25),
                          panel.grid.minor.x = element_blank(),
                          legend.position = "bottom",
                          legend.title = element_blank()),
                  tooltip = c("text")) %>% layout(legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.25),hovermode = "x")
  }
  
  return(g)
}

create_facet_bar_chart <- function(t, w.x, w.y, g, w.scales="free", w.facet=3, d=0, w.factor=1, s="", w.dec = 0, w.title, f, w.label=label_comma(), w.colors=psrc.colors, w.pos="dodge", lab.clr ="white", lab.sz=3) {
  
  c <- ggplot(data=t, 
              aes(y=get(eval(w.y)), 
                  x=get(eval(w.x)), 
                  fill = get(eval(f)),
              ))+
    geom_bar(position=w.pos, stat="identity") +
    geom_text(aes(label = paste0(prettyNum(round(get(eval(w.y))*w.factor,w.dec), big.mark = ","),s)), vjust = 1.5, colour = lab.clr, size=lab.sz) +
    scale_y_continuous(labels = w.label) +
    theme_light() +
    scale_fill_manual(values=w.colors) +
    theme(
      axis.text.y= element_blank(),
      axis.text.x= element_text(size = 8),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      panel.grid.major.y = element_line(colour="#BBBDC0",size = 0.25),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position="none",
      legend.title = element_blank())+
    facet_wrap(vars(get(eval(g))), scales=w.scales, ncol=w.facet)
  return(c)
}

create_bar_chart <- function(w.data, w.x, w.y, w.bartype, w.transparent, w.color, w.palette=psrc.colors, w.dec=0, w.suff="", w.labels = label_comma()) {  
  
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
                   scale_y_continuous(labels = w.labels) +
                   scale_fill_manual(values = w.palette) +
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

# Transit Trends ----------------------------------------------------------
psrc.transit <- c("1","3","5","20","23","29","35","40","54")
transit.agency.order <- c("Everett Transit", "Kitsap Transit", "City of Seattle", "Pierce Transit", "Community Transit", "Sound Transit", "King County Metro", "Washington State Ferries")
bus.modes <- c("CB","MB","TB")
ferry.modes <- c("FB")
light.rail.modes <- c("LR","SR","MG","MO")
commuter.rail.modes <- c("CR")

ntd.start.year <- 2016
ntd.end.year <- 2021
ntd.end.month <- 12

ntd.data <- as_tibble(read.xlsx(transit.file, sheet = "UPT", detectDates = TRUE, skipEmptyRows = TRUE, startRow = 1, colNames = TRUE))

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

monthly.transit <- ntd.psrc %>%
  select(Month, Estimate) %>%
  group_by(Month) %>%
  summarise(Estimate=sum(Estimate)) %>%
  mutate(year = as.character(year(Month))) %>%
  filter(year(Month) >= 2019) %>%
  mutate(day = ymd(paste0("2021-",month(Month),"-",day(Month))))

monthly.transit.chart <- create.line.chart(t=monthly.transit, w.x='day', w.y='Estimate', w.g='year', w.yr='year', 
                                  w.title='', w.lab=label_comma(), w.x.title='', x.type = 'Date', d.form = "%B")

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

transit.by.agency <- bind_rows(list(crt.by.agency, lrt.by.agency, fry.by.agency, bus.by.agency)) %>% filter(Year <= ntd.end.year)
transit.by.agency$Agency <- factor(transit.by.agency$Agency, levels=transit.agency.order)

current.colors <- c("Sound Transit" = "#005753",
                    "King County Metro" = "#9f3913",
                    "Community Transit" = "#8CC63E",
                    "Kitsap Transit" = "#91268F",
                    "Everett Transit" = "#C0E095",
                    "Pierce Transit" = "#C388C2",
                    "Washington State Ferries" = "#00A7A0",
                    "City of Seattle" = "#F7A489")

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
                                       scale_fill_manual(values = current.colors) +
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

# Airport Trends ----------------------------------------------------------
seatac.airport <- fread(file.path(srv.dir,'data','SEA activity measures by week - SEA measures.csv')) %>%
  select(1:5) %>%
  slice(2:n()) %>%
  setnames(c("Week","2022","2021","2020","2019")) %>%
  mutate(Week = gsub("Week ","",Week)) %>%
  mutate(Week  =as.integer(Week)) %>%
  pivot_longer(cols=c("2022","2021","2020","2019"), names_to = "year", values_to ="passenger_screenings") %>%
  mutate(day = (ymd( "2021-01-01" ) + weeks(Week-1))) %>%
  mutate(passenger_screenings = gsub(",","",passenger_screenings)) %>%
  mutate(passenger_screenings  =as.integer(passenger_screenings)) %>%
  mutate(year =as.character(year))

seatac.chart <- create.line.chart(t=seatac.airport, w.x='day', w.y='passenger_screenings', w.g='year', w.yr='year', 
                                  w.title='', w.lab=label_comma(), w.x.title='', x.type = 'Date', d.form = "%B")

# Employment Trends -------------------------------------------------------
qcew.areas <- c("Washington State", "Seattle MSA", "Tacoma MSA", "Bremerton MSA")

goods.producing <- c("Mining and Logging", "Construction", "Manufacturing")
private.services <- c("Trade, Transportation, and Utilities", "Information", "Financial Activities", "Professional and Business Services",
                      "Educational and Health Services", "Leisure and Hospitality", "Other Services")
government.services <- c("Government")
job.categories <- c(goods.producing, private.services,government.services)

qcew.jobs<- NULL
for (areas in qcew.areas) {

  t <- as_tibble(read.xlsx(file.path(srv.dir,'data','WA-QB-historical-SA.xlsx'), sheet = areas, detectDates = TRUE, skipEmptyRows = TRUE, startRow = 2, colNames = TRUE)) %>%
    pivot_longer(cols=contains("-"), names_to = "month", values_to ="jobs") %>%
    mutate(month = ymd(month)) %>%
    mutate(geography=areas) %>%
    select(-NAICS.CELL) %>%
    mutate(NAICS.INDUSTRY = trimws(NAICS.INDUSTRY, "both"))
  
  if (areas == "Bremerton MSA") {
    
    private.total <- t %>% filter(NAICS.INDUSTRY=="Private Service Providing") 
    detailed.private <- t %>% 
      filter(NAICS.INDUSTRY %in% private.services) %>%
      group_by(month) %>%
      summarize(detailed_jobs=sum(jobs))
    
    private.total <-left_join(private.total,detailed.private,by=c("month")) %>%
      mutate(jobs = jobs - detailed_jobs) %>%
      mutate(NAICS.INDUSTRY = "Other Services") %>%
      select(-detailed_jobs)
    
    t <- bind_rows(t, private.total)
    rm(private.total,detailed.private)
    
    t <- t %>%
      mutate(NAICS.INDUSTRY = gsub("Mining, Logging, and Construction", "Construction", NAICS.INDUSTRY))
    
  }
  
  ifelse(is.null(qcew.jobs), qcew.jobs <- t, qcew.jobs <- bind_rows(qcew.jobs,t))
  
  rm(t)
}

r <- qcew.jobs %>%
  filter(!(geography %in% c("Washington State"))) %>%
  group_by(NAICS.INDUSTRY,month) %>%
  summarize(jobs=sum(jobs)) %>%
  mutate(geography="Region")

qcew.jobs <- bind_rows(qcew.jobs,r) %>% 
  mutate(year = year(month), date = paste0(months(month),"-",year(month))) %>% 
  mutate(year=as.character(year))

rm(r)

tbl <- qcew.jobs %>% filter(NAICS.INDUSTRY=="Total Nonfarm") %>% filter(geography %in% c("Region", "Washington State")) %>% filter(year(month)>=2015)
total.jobs.chart <- create.line.chart(t=tbl, w.x='month', w.y='jobs', w.g='geography', w.yr='year', w.title='',
                                      w.lab=label_comma(), w.x.title='', x.type = 'Date', d.form="%b-%Y")
rm(tbl)

max.month <- month(qcew.jobs %>% select(month) %>% pull() %>% unique() %>% max())
tbl <- qcew.jobs %>% 
  filter(NAICS.INDUSTRY %in% sectors.of.interest) %>% 
  filter(geography %in% c("Region")) %>% 
  filter(year(month)>=2018)%>% filter(month(month)==max.month) %>%
  mutate(NAICS.INDUSTRY = factor(NAICS.INDUSTRY, levels = c("Manufacturing", "Construction", "Leisure and Hospitality", "Other Services", "Professional and Business Services", "Educational and Health Services")))

jobs.by.sector.chart <- create_facet_bar_chart(t=tbl, w.x='year', w.y='jobs', g='NAICS.INDUSTRY',w.title='Jobs', f='NAICS.INDUSTRY', w.facet=2)
rm(tbl)

# Work Force by Category --------------------------------------------------
# By Gender
gender.workers <- as_tibble(read.xlsx(gender.file, sheet = "Data", detectDates = TRUE, skipEmptyRows = TRUE, startRow = 1, colNames = TRUE))

gender.workers <- gender.workers %>%
  rename(Quarter=X1) %>%
  select(-contains("Flags")) %>%
  pivot_longer(cols = -contains("Quarter"), names_to="Gender", values_to="Estimate") %>%
  mutate(Quarter = gsub(" ",":",Quarter), Gender = gsub("\\."," ",Gender)) %>%
  mutate(Quarter = yq(Quarter))

total.workers <- gender.workers %>%
  select(-Gender) %>%
  group_by(Quarter) %>%
  summarize_all(sum) %>%
  mutate(Gender="Total")

workforce.by.gender <- bind_rows(list(gender.workers, total.workers)) 

workforce.by.gender <- workforce.by.gender %>%
  filter(Quarter=="2020-01-01") %>%
  select(-Quarter) %>%
  rename(WorkForce=Estimate)

# By Race
non.hispanic.workers <- as_tibble(read.xlsx(nonhispanic.file, sheet = "Data", detectDates = TRUE, skipEmptyRows = TRUE, startRow = 1, colNames = TRUE))

non.hispanic.workers <- non.hispanic.workers %>%
  rename(Quarter=X1) %>%
  select(-contains("Flags")) %>%
  pivot_longer(cols = -contains("Quarter"), names_to="Race", values_to="Estimate") %>%
  mutate(Quarter = gsub(" ",":",Quarter), Race = gsub("\\."," ",Race)) %>%
  mutate(Quarter = yq(Quarter))

hispanic.workers <- as_tibble(read.xlsx(hispanic.file, sheet = "Data", detectDates = TRUE, skipEmptyRows = TRUE, startRow = 1, colNames = TRUE))

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
  filter(Quarter=="2020-01-01") %>%
  select(-Quarter) %>%
  rename(WorkForce=Estimate)

# By Age
age.workers <- as_tibble(read.xlsx(age.file, sheet = "Data", detectDates = TRUE, skipEmptyRows = TRUE, startRow = 1, colNames = TRUE))

age.workers <- age.workers %>%
  rename(Quarter=X1) %>%
  select(-contains("Flags")) %>%
  pivot_longer(cols = contains("-"), names_to="Age", values_to="Estimate") %>%
  mutate(Quarter = gsub(" ",":",Quarter), Age = gsub("\\."," ",Age)) %>%
  mutate(Quarter = yq(Quarter))

total.workers <- age.workers %>%
  select(-Age) %>%
  group_by(Quarter) %>%
  summarize_all(sum) %>%
  mutate(Age="Total")

workforce.by.age <- bind_rows(list(age.workers, total.workers)) 

workforce.by.age <- workforce.by.age %>%
  filter(Quarter=="2020-01-01") %>%
  select(-Quarter) %>%
  rename(WorkForce=Estimate)

t <- workforce.by.age %>%
  filter(Age %in% c("19-21","22-24")) %>%
  mutate(Age="18-24") %>%
  group_by(Age) %>%
  summarize(WorkForce=sum(WorkForce))

workforce.by.age <- bind_rows(workforce.by.age,t)
rm(t)

workforce.by.age <- workforce.by.age %>%
  mutate(Age=gsub("65-99","65+",Age)) %>%
  filter(Age %in% c("18-24","25-34","35-44","45-54","55-64","65+","Total"))

# By Education
education.workers <- as_tibble(read.xlsx(edu.file, sheet = "Data", detectDates = TRUE, skipEmptyRows = TRUE, startRow = 1, colNames = TRUE))

education.workers <- education.workers %>%
  rename(Quarter=X1) %>%
  select(-contains("Flags")) %>%
  pivot_longer(cols = contains("."), names_to="Education", values_to="Estimate") %>%
  mutate(Quarter = gsub(" ",":",Quarter), Education = gsub("\\."," ",Education)) %>%
  mutate(Quarter = yq(Quarter))

total.workers <- education.workers %>%
  select(-Education) %>%
  group_by(Quarter) %>%
  summarize_all(sum) %>%
  mutate(Education="Total")

workforce.by.education <- bind_rows(list(education.workers, total.workers)) 

workforce.by.education <- workforce.by.education %>%
  filter(Quarter=="2020-01-01") %>%
  select(-Quarter) %>%
  rename(WorkForce=Estimate)

workforce.by.education <- left_join(workforce.by.education, education.lookup, by=c("Education"="name")) %>%
  mutate(Education=value) %>%
  select(-value)

# Unemployment Trends for Initial Claims during the Pandemic ------------------------------------------------------------
intial.claims.gender <- as_tibble(read.xlsx(init.claims.file, sheet = "Claimants by gender", detectDates = TRUE, skipEmptyRows = TRUE, startRow = 6, colNames = TRUE)) %>% 
  slice(1:3) %>%
  select(Gender.of.Claimant, King.County, Kitsap.County, Pierce.County, Snohomish.County) %>%
  pivot_longer(cols=contains(".County")) %>%
  group_by(Gender.of.Claimant) %>%
  summarize(initial_claims=sum(value)) %>%
  filter(Gender.of.Claimant != "Total, All Unduplicated Claimants") %>%
  rename(demographic = Gender.of.Claimant) %>%
  mutate(category="Gender")

intial.claims.race <- as_tibble(read.xlsx(init.claims.file, sheet = "Claimants by race and ethnicity", detectDates = TRUE, skipEmptyRows = TRUE, startRow = 6, colNames = TRUE)) %>% 
  slice(3:9) %>%
  select(`Race/Ethnicity.of.Claimant`, King.County, Kitsap.County, Pierce.County, Snohomish.County) %>%
  pivot_longer(cols=contains(".County")) %>%
  mutate(`Race/Ethnicity.of.Claimant` = trimws(`Race/Ethnicity.of.Claimant`, "both")) %>%
  group_by(`Race/Ethnicity.of.Claimant`) %>%
  summarize(initial_claims=sum(value)) %>%
  filter(`Race/Ethnicity.of.Claimant` != "Total, All Unduplicated Claimants") %>%
  rename(demographic = `Race/Ethnicity.of.Claimant`) %>%
  mutate(demographic = gsub("African American","Black or African American Alone",demographic)) %>%
  mutate(demographic = gsub("American Indian","American Indian or Alaska Native Alone",demographic)) %>%
  mutate(demographic = gsub("Asian","Asian Alone",demographic)) %>%
  mutate(demographic = gsub("Caucasian","White Alone",demographic)) %>%
  mutate(demographic = gsub("Latino/Hispanic of any race","Hispanic or Latino",demographic)) %>%
  mutate(demographic = gsub("Pacific Islander","Native Hawaiian or Other Pacific Islander Alone",demographic)) %>%
  mutate(demographic = gsub("Two or More Races","Two or More Race Groups",demographic))%>%
  mutate(category="Race")

intial.claims.age <- as_tibble(read.xlsx(init.claims.file, sheet = "Claimants by age", detectDates = TRUE, skipEmptyRows = TRUE, startRow = 6, colNames = TRUE)) %>% 
  slice(2:9) %>%
  select(Age.of.Claimant, King.County, Kitsap.County, Pierce.County, Snohomish.County) %>%
  pivot_longer(cols=contains(".County")) %>%
  mutate(Age.of.Claimant = trimws(Age.of.Claimant, "both")) %>%
  group_by(Age.of.Claimant) %>%
  summarize(initial_claims=sum(value)) %>%
  rename(demographic = Age.of.Claimant) %>%
  filter(demographic != "<18") %>%
  filter(demographic != "<24")%>%
  mutate(category="Age")

intial.claims.education <- as_tibble(read.xlsx(init.claims.file, sheet = "Claimants by education", detectDates = TRUE, skipEmptyRows = TRUE, startRow = 6, colNames = TRUE)) %>% 
  slice(2:11) %>%
  select(Formal.Education.of.Claimant, King.County, Kitsap.County, Pierce.County, Snohomish.County) %>%
  pivot_longer(cols=contains(".County")) %>%
  mutate(Formal.Education.of.Claimant = trimws(Formal.Education.of.Claimant, "both")) %>%
  group_by(Formal.Education.of.Claimant) %>%
  summarize(initial_claims=sum(value)) %>%
  rename(demographic = Formal.Education.of.Claimant)%>%
  mutate(category="Education")

intial.claims.education <- left_join(intial.claims.education, education.lookup, by=c("demographic"="name")) %>%
  mutate(demographic=value) %>%
  select(-value) %>%
  group_by(demographic, category) %>%
  summarize(initial_claims = sum(initial_claims))

initial.claims <- bind_rows(list(intial.claims.gender, intial.claims.race, intial.claims.age, intial.claims.education))

# Workforce
workforce.by.race <- workforce.by.race %>% rename(demographic=Race)
workforce.by.gender <- workforce.by.gender %>% rename(demographic=Gender)
workforce.by.age <- workforce.by.age %>% rename(demographic=Age)
workforce.by.education <- workforce.by.education %>% rename(demographic=Education)

workforce <- bind_rows(workforce.by.race, workforce.by.gender, workforce.by.age, workforce.by.education) %>% filter(demographic != "Total")

initial.claims <- left_join(initial.claims, workforce, by=c("demographic"))

initial.claims <- initial.claims %>%
  mutate(share = (initial_claims / WorkForce)) %>%
  mutate(demographic_titles=str_wrap(demographic,width=10)) %>%
  filter(demographic != "Under age 24")

tbl <- initial.claims %>%
  filter(demographic %in% c("Female","Male", "Black or African American Alone","American Indian or Alaska Native Alone",
                            "Asian Alone", "White Alone", "Hispanic or Latino","Native Hawaiian or Other Pacific Islander Alone",
                            "Two or More Race Groups")) %>%
  mutate(demographic_titles = gsub("Alone", "", demographic_titles)) %>%
  mutate(demographic_titles = gsub("Latino", "Latinx", demographic_titles))


initial.claims.share.race.chart <- create_facet_bar_chart(t=tbl, w.x='demographic_titles', w.y='share', g='category',
                                               w.title='Initial Claims', f='demographic', w.facet=1, w.factor = 100, s="%")

tbl <- initial.claims %>%
  filter(demographic %in% c("18-24","25-34", "35-44","45-54","55-64", "65+", 
                            "College degree","High School", "Less than high school", "Some College"))


initial.claims.share.age.chart <- create_facet_bar_chart(t=tbl, w.x='demographic_titles', w.y='share', g='category',
                                                          w.title='Initial Claims', f='demographic', w.facet=1, w.factor = 100, s="%")

# OFM Population Data -----------------------------------------------------
ofm.pop <- as_tibble(read.xlsx(ofm.pop.file, detectDates = TRUE, skipEmptyRows = TRUE, startRow = 5, colNames = TRUE))

# Get rid of things we don't need, make it long form and clean it up for use in plot making
ofm.pop <- ofm.pop %>%
  filter(County %in%  c(psrc.counties,"State")) %>%
  pivot_longer(cols=contains("Population"), names_to="Year", values_to="Estimate") %>%
  select(-Line) %>%
  mutate(Year = str_replace(Year, ".Population.*", ""), Jurisdiction = str_replace(Jurisdiction, " \\(part\\)", "")) %>%
  mutate(across(c('Filter','Year','Estimate'), as.numeric))

# Combine Places like Auburn, Bothell and Pacific into One Total
ofm.pop <- ofm.pop %>%
  select(-County) %>%
  group_by(Filter,Jurisdiction,Year) %>%
  summarize(Estimate = sum(Estimate))

# Create a Regional Summary by Filter Type and then Join to Full OFM tibble
region.pop <- ofm.pop %>%
  filter(Filter <= 3) %>%
  select(Filter,Year, Estimate) %>%
  group_by(Filter,Year) %>%
  summarize(Estimate = sum(Estimate)) %>%
  mutate(Jurisdiction = "Region") %>%
  mutate(Jurisdiction = ifelse(Filter == 2, "Unincorporated Region Total", Jurisdiction)) %>%
  mutate(Jurisdiction = ifelse(Filter == 3, "Incorporated Region Total", Jurisdiction))

# Add the regional results to the OFM full tibble
ofm.pop <- bind_rows(ofm.pop,region.pop) %>% mutate(Variable="Total Population")

# Process Population change components - Natural Increase
ofm.pop.chg.natural <- as_tibble(read.xlsx(ofm.pop.chg.file, detectDates = TRUE, skipEmptyRows = TRUE, startRow = 4, colNames = TRUE, sheet = "Natural Increase"))

# Get rid of things we don't need, make it long form and clean it up for use in plot making
ofm.pop.chg.natural <- ofm.pop.chg.natural %>%
  filter(County %in%  c(psrc.counties,"State")) %>%
  pivot_longer(cols=contains("Natural.Increase"), names_to="Year", values_to="Estimate") %>%
  select(-Spacer) %>%
  mutate(Year = str_replace(Year, "Natural.Increase.", ""), Jurisdiction = str_replace(Jurisdiction, " \\(part\\)", "")) %>%
  separate(Year, c("temp", "Year"), "-") %>%
  select(-temp) %>%
  mutate(Year = str_replace(Year, "20211", "2021")) %>%
  mutate(across(c('Year','Estimate'), as.numeric)) %>%
  mutate(Filter=4)

# Create a Regional Summary by Filter Type and then Join to Full OFM tibble
region.pop <- ofm.pop.chg.natural %>%
  filter(County != "State") %>%
  select(Year, Estimate) %>%
  group_by(Year) %>%
  summarize_all(sum) %>%
  mutate(County = "Region") %>%
  mutate(Jurisdiction = "Region", Filter=5)

# Add the regional results to the OFM full tibble
ofm.pop.chg.natural <- bind_rows(ofm.pop.chg.natural,region.pop) %>% mutate(Variable="Natural Increase") %>% select(-County)

# Process Population change components - Migration
ofm.pop.chg.migration <- as_tibble(read.xlsx(ofm.pop.chg.file, detectDates = TRUE, skipEmptyRows = TRUE, startRow = 4, colNames = TRUE, sheet = "Residual Net Migration"))

# Get rid of things we don't need, make it long form and clean it up for use in plot making
ofm.pop.chg.migration <- ofm.pop.chg.migration %>%
  filter(County %in%  c(psrc.counties,"State")) %>%
  pivot_longer(cols=contains("Residual.Net.Migration"), names_to="Year", values_to="Estimate") %>%
  select(-Spacer) %>%
  mutate(Year = str_replace(Year, "Residual.Net.Migration.", ""), Jurisdiction = str_replace(Jurisdiction, " \\(part\\)", "")) %>%
  separate(Year, c("temp", "Year"), "-") %>%
  select(-temp) %>%
  mutate(Year = str_replace(Year, "20211", "2021")) %>%
  mutate(across(c('Year','Estimate'), as.numeric)) %>%
  mutate(Filter=4)

# Create a Regional Summary by Filter Type and then Join to Full OFM tibble
region.pop <- ofm.pop.chg.migration %>%
  filter(County != "State") %>%
  select(Year, Estimate) %>%
  group_by(Year) %>%
  summarize_all(sum) %>%
  mutate(County = "Region") %>%
  mutate(Jurisdiction = "Region", Filter=5)

# Add the regional results to the OFM full tibble
ofm.pop.chg.migration <- bind_rows(ofm.pop.chg.migration,region.pop) %>% mutate(Variable="Migration") %>% select(-County)

# Process Population change components - Total Change
ofm.pop.chg.total <- as_tibble(read.xlsx(ofm.pop.chg.file, detectDates = TRUE, skipEmptyRows = TRUE, startRow = 4, colNames = TRUE, sheet = "Annual Change"))

# Get rid of things we don't need, make it long form and clean it up for use in plot making
ofm.pop.chg.total <- ofm.pop.chg.total %>%
  filter(County %in%  c(psrc.counties,"State")) %>%
  pivot_longer(cols=contains("Annual.Change.in.Total.Population"), names_to="Year", values_to="Estimate") %>%
  select(-Spacer) %>%
  mutate(Year = str_replace(Year, "Annual.Change.in.Total.Population.", ""), Jurisdiction = str_replace(Jurisdiction, " \\(part\\)", "")) %>%
  separate(Year, c("temp", "Year"), "-") %>%
  select(-temp) %>%
  mutate(Year = str_replace(Year, "20211", "2021")) %>%
  mutate(across(c('Year','Estimate'), as.numeric)) %>%
  mutate(Filter=4)

# Create a Regional Summary by Filter Type and then Join to Full OFM tibble
region.pop <- ofm.pop.chg.total %>%
  filter(County != "State") %>%
  select(Year, Estimate) %>%
  group_by(Year) %>%
  summarize_all(sum) %>%
  mutate(County = "Region") %>%
  mutate(Jurisdiction = "Region", Filter=5)

# Add the regional results to the OFM full tibble
ofm.pop.chg.total <- bind_rows(ofm.pop.chg.total,region.pop) %>% mutate(Variable="Total Change") %>% select(-County)

# Combine components of change into one tibble
ofm.pop <- bind_rows(list(ofm.pop,ofm.pop.chg.natural, ofm.pop.chg.migration, ofm.pop.chg.total))

# Housing Units
ofm.housing <- as_tibble(read.xlsx(ofm.housing.file, sheet = "Housing Units", detectDates = TRUE, skipEmptyRows = TRUE, startRow = 4, colNames = TRUE))

# Get rid of things we don't need, make it long form and clean it up for use in plot making
ofm.housing <- ofm.housing %>%
  filter(County %in%  c(psrc.counties)) %>%
  mutate(across(contains(".Housing.Units"), as.character)) %>%
  pivot_longer(cols=contains(".Housing.Units"), names_to="Year", values_to="Estimate") %>%
  select(-Line) %>%
  mutate(Year = str_replace(Year, ".Census.Count.of.", "-"), Year = str_replace(Year, ".Postcensal.Estimate.of.", "-"), Year = str_replace(Year, ".Census-Based.Estimate.of.", "-"), Jurisdiction = str_replace(Jurisdiction, " \\(part\\)", "")) %>%
  separate(Year, c("Year", "Variable"), "-") %>%
  mutate(across(c('Year'), as.numeric)) %>%
  mutate(across(c('Estimate'), as.numeric)) %>%
  mutate(Variable = gsub("\\."," ",Variable)) %>%
  select(-County) %>%
  mutate(Estimate = case_when(
    is.na(Estimate) ~ 0,
    !(is.na(Estimate)) ~ Estimate)) %>%
  group_by(Filter, Jurisdiction, Year, Variable) %>%
  summarize(Estimate=sum(Estimate))

ofm.pop <- as_tibble(ofm.pop)
ofm.housing <- as_tibble(ofm.housing)

# Create Region Total
region.housing <- ofm.housing %>%
  filter(Filter == "1") %>%
  group_by(Year,Variable) %>%
  summarize(Estimate=sum(Estimate)) %>%
  mutate(Filter=5, Jurisdiction="Region")

region.housing <- as_tibble(region.housing)

ofm.housing <- bind_rows(ofm.housing, region.housing) %>% mutate(Filter=as.numeric(Filter))

ofm.data <- bind_rows(ofm.pop, ofm.housing)

tbl <- ofm.data %>%
  filter(Variable %in% c("Migration", "Natural Increase") & Jurisdiction == "Region" & Year >2010) %>%
  mutate(Year = as.character(Year))

region.pop.change.chart <- create_bar_chart(w.data = tbl, w.x = "Year", w.y = "Estimate", w.bartype = "stack", 
                                            w.transparent = 1.0, w.color = "Variable")

# City Changes
tbl <- ofm.data %>%
  filter(Variable=="Total Population" & Jurisdiction %in% city.of.interest & Year %in% c("2015","2018","2020","2021")) %>%
  mutate(Year = as.character(Year)) %>%
  mutate(Jurisdiction = factor(Jurisdiction, levels = c("Bremerton","Bellevue","Everett","Seattle","Tacoma")))

city.pop.change.chart <- create_facet_bar_chart(t=tbl, w.x='Year', w.y='Estimate', g='Jurisdiction',
                                                w.title='', f='Year', w.facet=3, w.scales="fixed", lab.clr="white", lab.sz=2)

# County Changes
tbl <- ofm.data %>%
  filter(Variable=="Total Population" & Jurisdiction %in% county.of.interest & Year %in% c("2015","2018","2020","2021")) %>%
  mutate(Year = as.character(Year)) %>%
  mutate(Jurisdiction = factor(Jurisdiction, levels = c("Kitsap County", "King County", "Pierce County", "Snohomish County")))

county.pop.change.chart <- create_facet_bar_chart(t=tbl, w.x='Year', w.y='Estimate', g='Jurisdiction',
                                                w.title='', f='Year', w.facet=2, w.scales="fixed", lab.clr="white", lab.sz=2)

# Housing Unit Trends ----------------------------------------------------------
tbl <- ofm.data %>%
  filter(Variable %in% c("Total Housing Units", "One Unit Housing Units", "Two or More Housing Units", "Mobile Home and Special Housing Units")) %>%
  filter(Year >= 2010 & Jurisdiction=="Region") %>%
  mutate(Year = as.character(Year)) %>%
  filter(Variable != "Total Housing Units") %>%
  mutate(Unit_Type = case_when(
    Variable == "One Unit Housing Units" ~ "Single-Family",
    Variable == "Two or More Housing Units" ~ "Multi-Family",
    Variable == "Mobile Home and Special Housing Units" ~ "Mobile-Home")) %>%
  mutate(Unit_Type = factor(Unit_Type, levels = c("Single-Family","Multi-Family","Mobile-Home"))) %>%
  arrange(Unit_Type, Year) %>%
  mutate(Delta = Estimate - lag(Estimate)) %>%
  filter(Year >= 2011)

region.hu.chart <- create_bar_chart(w.data = tbl, w.x = "Year", w.y = "Delta", w.bartype = "stack", 
                                            w.transparent = 1.0, w.color = "Unit_Type")
# City
tbl <- ofm.data %>%
  filter(Variable=="Total Housing Units" & Jurisdiction %in% city.of.interest & Year %in% c("2015","2018","2020","2021")) %>%
  mutate(Year = as.character(Year)) %>%
  mutate(Jurisdiction = factor(Jurisdiction, levels = c("Bremerton","Bellevue","Everett","Seattle","Tacoma")))

city.hu.change.chart <- create_facet_bar_chart(t=tbl, w.x='Year', w.y='Estimate', g='Jurisdiction',
                                                w.title='', f='Year', w.facet=3, w.scales="fixed", lab.clr="white", lab.sz=2)

# County
tbl <- ofm.data %>%
  filter(Variable=="Total Housing Units" & Jurisdiction %in% county.of.interest & Year %in% c("2015","2018","2020","2021")) %>%
  mutate(Year = as.character(Year)) %>%
  mutate(Jurisdiction = factor(Jurisdiction, levels = c("Kitsap County", "King County", "Pierce County", "Snohomish County")))

county.hu.change.chart <- create_facet_bar_chart(t=tbl, w.x='Year', w.y='Estimate', g='Jurisdiction',
                                               w.title='', f='Year', w.facet=2, w.scales="fixed", lab.clr="white", lab.sz=2)

# Rentals -----------------------------------------------------------------
region.rentals <- as_tibble(fread(rentals.file))%>% 
  filter(RegionName == "Seattle, WA") %>% 
  select(-RegionID, -SizeRank) %>% 
  pivot_longer(cols=contains("-"), names_to="Year", values_to="Estimate") %>%
  mutate(Year = str_replace(Year, "X", "")) %>%
  mutate(Year = ym(Year))

region.rentals.chart <- create_bar_chart(w.data = region.rentals, w.x = "Year", w.y = "Estimate", w.bartype = "dodge", 
                                         w.transparent = 1.0, w.color = "RegionName", w.labels = label_dollar())

# Home Sales --------------------------------------
home.sales <- as_tibble(read.xlsx(home.sales.file, sheet = "Sheet 1", detectDates = TRUE, skipEmptyRows = TRUE, startRow = 1, colNames = TRUE))

median.price <- home.sales %>% select(Month.of.Period.End, Median.Sale.Price) %>% mutate(Month.of.Period.End = my(Month.of.Period.End)) %>% rename(Month=Month.of.Period.End) %>% mutate(Variable="Median Sales Price") 
homes.sold <- home.sales %>% select(Month.of.Period.End, Homes.Sold) %>% mutate(Month.of.Period.End = my(Month.of.Period.End)) %>% rename(Month=Month.of.Period.End) %>% mutate(Variable="Homes Sold") 
homes.inventory <- home.sales %>% select(Month.of.Period.End, Inventory) %>% mutate(Month.of.Period.End = my(Month.of.Period.End)) %>% rename(Month=Month.of.Period.End) %>% mutate(Variable="Home Inventory") 

region.mediansales.chart <- create_bar_chart(w.data = median.price, w.x = "Month", w.y = "Median.Sale.Price", w.bartype = "dodge", w.transparent = 1.0, w.color = "Variable", w.labels = label_dollar())
region.homesales.chart <- create_bar_chart(w.data = homes.sold, w.x = "Month", w.y = "Homes.Sold", w.bartype = "dodge", w.transparent = 1.0, w.color = "Variable")
region.homeinventory.chart <- create_bar_chart(w.data = homes.inventory, w.x = "Month", w.y = "Inventory", w.bartype = "dodge", w.transparent = 1.0, w.color = "Variable")

# Congestion --------------------------------------------------------------
temp <- fread(file.path(srv.dir,'data','May_2019_Cars_tmc_95th_percentile_speed.csv')) %>%
  select(thrulanes,miles,contains("_ratio")) %>%
  mutate(lane_miles=thrulanes*miles)

total.miles <- temp %>% select(lane_miles) %>% drop_na() %>% pull() %>% sum()
am5 <- temp %>% select(lane_miles, `5am_ratio`) %>% filter(`5am_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
am6 <- temp %>% select(lane_miles, `6am_ratio`) %>% filter(`6am_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
am7 <- temp %>% select(lane_miles, `7am_ratio`) %>% filter(`7am_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
am8 <- temp %>% select(lane_miles, `8am_ratio`) %>% filter(`8am_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
am9 <- temp %>% select(lane_miles, `9am_ratio`) %>% filter(`9am_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
am10 <- temp %>% select(lane_miles, `10am_ratio`) %>% filter(`10am_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
am11 <- temp %>% select(lane_miles, `11am_ratio`) %>% filter(`11am_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
pm12 <- temp %>% select(lane_miles, `Noon_ratio`) %>% filter(`Noon_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
pm1 <- temp %>% select(lane_miles, `1pm_ratio`) %>% filter(`1pm_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
pm2 <- temp %>% select(lane_miles, `2pm_ratio`) %>% filter(`2pm_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
pm3 <- temp %>% select(lane_miles, `3pm_ratio`) %>% filter(`3pm_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
pm4 <- temp %>% select(lane_miles, `4pm_ratio`) %>% filter(`4pm_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
pm5 <- temp %>% select(lane_miles, `5pm_ratio`) %>% filter(`5pm_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
pm6 <- temp %>% select(lane_miles, `6pm_ratio`) %>% filter(`6pm_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
pm7 <- temp %>% select(lane_miles, `7pm_ratio`) %>% filter(`7pm_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
pm8 <- temp %>% select(lane_miles, `8pm_ratio`) %>% filter(`8pm_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()

congestion.2019 <- data.table(Year="2019", 
                         `Time of Day`=c('5am','6am','7am','8am','9am','10am','11am','Noon','1pm','2pm','3pm','4pm','5pm','6pm','7pm','8pm'),
                         Congested_Miles=c(am5, am6, am7, am8, am9, am10, am11, pm12, pm1, pm2, pm3, pm4, pm5, pm6, pm7, pm8))

temp <- fread(file.path(srv.dir,'data','May_2021_Cars_tmc_95th_percentile_speed.csv')) %>%
  select(thrulanes,miles,contains("_ratio")) %>%
  mutate(lane_miles=thrulanes*miles)

total.miles <- temp %>% select(lane_miles) %>% drop_na() %>% pull() %>% sum()
am5 <- temp %>% select(lane_miles, `5am_ratio`) %>% filter(`5am_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
am6 <- temp %>% select(lane_miles, `6am_ratio`) %>% filter(`6am_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
am7 <- temp %>% select(lane_miles, `7am_ratio`) %>% filter(`7am_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
am8 <- temp %>% select(lane_miles, `8am_ratio`) %>% filter(`8am_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
am9 <- temp %>% select(lane_miles, `9am_ratio`) %>% filter(`9am_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
am10 <- temp %>% select(lane_miles, `10am_ratio`) %>% filter(`10am_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
am11 <- temp %>% select(lane_miles, `11am_ratio`) %>% filter(`11am_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
pm12 <- temp %>% select(lane_miles, `Noon_ratio`) %>% filter(`Noon_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
pm1 <- temp %>% select(lane_miles, `1pm_ratio`) %>% filter(`1pm_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
pm2 <- temp %>% select(lane_miles, `2pm_ratio`) %>% filter(`2pm_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
pm3 <- temp %>% select(lane_miles, `3pm_ratio`) %>% filter(`3pm_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
pm4 <- temp %>% select(lane_miles, `4pm_ratio`) %>% filter(`4pm_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
pm5 <- temp %>% select(lane_miles, `5pm_ratio`) %>% filter(`5pm_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
pm6 <- temp %>% select(lane_miles, `6pm_ratio`) %>% filter(`6pm_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
pm7 <- temp %>% select(lane_miles, `7pm_ratio`) %>% filter(`7pm_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
pm8 <- temp %>% select(lane_miles, `8pm_ratio`) %>% filter(`8pm_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()

congestion.2021 <- data.table(Year="2021", 
                              `Time of Day`=c('5am','6am','7am','8am','9am','10am','11am','Noon','1pm','2pm','3pm','4pm','5pm','6pm','7pm','8pm'),
                         Congested_Miles=c(am5, am6, am7, am8, am9, am10, am11, pm12, pm1, pm2, pm3, pm4, pm5, pm6, pm7, pm8))

temp <- fread(file.path(srv.dir,'data','May_2020_Cars_tmc_95th_percentile_speed.csv')) %>%
  select(thrulanes,miles,contains("_ratio")) %>%
  mutate(lane_miles=thrulanes*miles)

total.miles <- temp %>% select(lane_miles) %>% drop_na() %>% pull() %>% sum()
am5 <- temp %>% select(lane_miles, `5am_ratio`) %>% filter(`5am_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
am6 <- temp %>% select(lane_miles, `6am_ratio`) %>% filter(`6am_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
am7 <- temp %>% select(lane_miles, `7am_ratio`) %>% filter(`7am_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
am8 <- temp %>% select(lane_miles, `8am_ratio`) %>% filter(`8am_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
am9 <- temp %>% select(lane_miles, `9am_ratio`) %>% filter(`9am_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
am10 <- temp %>% select(lane_miles, `10am_ratio`) %>% filter(`10am_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
am11 <- temp %>% select(lane_miles, `11am_ratio`) %>% filter(`11am_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
pm12 <- temp %>% select(lane_miles, `Noon_ratio`) %>% filter(`Noon_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
pm1 <- temp %>% select(lane_miles, `1pm_ratio`) %>% filter(`1pm_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
pm2 <- temp %>% select(lane_miles, `2pm_ratio`) %>% filter(`2pm_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
pm3 <- temp %>% select(lane_miles, `3pm_ratio`) %>% filter(`3pm_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
pm4 <- temp %>% select(lane_miles, `4pm_ratio`) %>% filter(`4pm_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
pm5 <- temp %>% select(lane_miles, `5pm_ratio`) %>% filter(`5pm_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
pm6 <- temp %>% select(lane_miles, `6pm_ratio`) %>% filter(`6pm_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
pm7 <- temp %>% select(lane_miles, `7pm_ratio`) %>% filter(`7pm_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()
pm8 <- temp %>% select(lane_miles, `8pm_ratio`) %>% filter(`8pm_ratio` <= 0.50) %>% drop_na() %>% pull() %>% sum()

congestion.2020 <- data.table(Year="2020", 
                              `Time of Day`=c('5am','6am','7am','8am','9am','10am','11am','Noon','1pm','2pm','3pm','4pm','5pm','6pm','7pm','8pm'),
                              Congested_Miles=c(am5, am6, am7, am8, am9, am10, am11, pm12, pm1, pm2, pm3, pm4, pm5, pm6, pm7, pm8))


congestion <- bind_rows(list(congestion.2019, congestion.2020, congestion.2021)) %>% 
  mutate(`Percent Heavy Congestion` =Congested_Miles/total.miles) %>%
  mutate(`Time of Day` = factor(`Time of Day`, levels = c('5am','6am','7am','8am','9am','10am','11am','Noon','1pm','2pm','3pm','4pm','5pm','6pm','7pm','8pm')))

congestion.chart <- ggplotly(ggplot(data=congestion, 
                                             aes(x=`Time of Day`,
                                                 y=`Percent Heavy Congestion`, 
                                                 group=Year,
                                                 text = paste0("<b>Year: </b>",  Year, "<br>","<b>",": </b>", prettyNum(round(`Percent Heavy Congestion`*100, 1), big.mark = ","), "%", "<br>")))  + 
                                        geom_line(aes(color=Year), size = 1)+
                               scale_color_manual(values = psrc.colors) +
                               scale_y_continuous(labels = label_percent()) +
                                        theme(plot.title = element_text(size = 10, face = 'bold'),
                                              axis.line.x = element_line(colour="#BBBDC0",size = 0.25),
                                              axis.line.y = element_line(colour="#BBBDC0",size = 0.25),
                                              panel.background = element_blank(),
                                              panel.grid.major.y = element_line(colour="#BBBDC0",size = 0.25),
                                              panel.grid.minor.y = element_blank(),
                                              panel.grid.major.x = element_line(colour="#BBBDC0",size = 0.25),
                                              panel.grid.minor.x = element_blank(),
                                              legend.position = "bottom",
                                              legend.title = element_blank()),
                                      tooltip = c("text")) %>% layout(legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.25),hovermode = "x")

# VMT --------------------------------------------------------------
vmt <- as_tibble(read.xlsx(file.path(srv.dir,'data','vmt-data.xlsx'), sheet = "Sheet1", detectDates = TRUE, skipEmptyRows = TRUE, startRow = 1, colNames = TRUE)) %>%
  mutate(Year = as.character(Year))

t <- vmt %>% filter(Type %in% "Statewide")

statewide.vmt.chart <- ggplot(data=t, 
                                    aes(x=`Year`,
                                        y=`Estimate`, 
                                        fill = `Type`))  + 
                        geom_col(position='dodge') +
                               scale_color_manual(values = psrc.colors) +
                               scale_y_continuous(labels = label_comma()) +
                               theme(plot.title = element_text(size = 10, face = 'bold'),
                                     axis.line.x = element_line(colour="#BBBDC0",size = 0.25),
                                     axis.line.y = element_line(colour="#BBBDC0",size = 0.25),
                                     panel.background = element_blank(),
                                     panel.grid.major.y = element_line(colour="#BBBDC0",size = 0.25),
                                     panel.grid.minor.y = element_blank(),
                                     panel.grid.major.x = element_line(colour="#BBBDC0",size = 0.25),
                                     panel.grid.minor.x = element_blank(),
                                     legend.position = "bottom",
                                     legend.title = element_blank())

# Highway Volumes ---------------------------------------------------------
volumes <- fread(file.path(srv.dir,'data','VolumeNumTableCountLocation_data.csv')) %>%
  filter(Counties %in% psrc.counties) %>%
  filter(`Measure Names` %in% c("Current")) %>% 
  mutate(Ratio = `Measure Values`/ `2019 Volume`) %>%
  rename(Location=`Traffic Count Locations`, Date=`Day of 2020Date`) %>%
  mutate(Date = str_sub(Date, 6,13)) %>%
  mutate(Date = mdy(Date))

i405.volumes <- volumes %>% filter(Highway1 %in% c("I-405")) %>% select(Location,Date, Ratio)
i405.chart <- create.line.chart(t=i405.volumes, w.x='Date', w.y='Ratio', w.g='Location', w.yr='Date', 
                                  w.title='', w.lab=label_percent(), w.x.title='', x.type = 'Date', d.form = "%B") 
  
i5.volumes <- volumes %>% filter(Highway1 %in% c("I-5")) %>% select(Location,Date, Ratio) %>% filter(Ratio <=2)
i5.chart <- create.line.chart(t=i5.volumes, w.x='Date', w.y='Ratio', w.g='Location', w.yr='Date', 
                                w.title='', w.lab=label_percent(), w.x.title='', x.type = 'Date', d.form = "%B") 

bridge.volumes <- volumes %>% filter(Location %in% c("SR 16 at Tacoma Narrows","SR 520 at SR 520 Floating Bridge", "I-90 at West of Bandera")) %>% select(Location,Date, Ratio) %>% filter(Ratio <=2 & Ratio >=0.1)
bridge.chart <- create.line.chart(t=bridge.volumes, w.x='Date', w.y='Ratio', w.g='Location', w.yr='Date', 
                                 w.title='', w.lab=label_percent(), w.x.title='', x.type = 'Date', d.form = "%B") 


other.volumes <- volumes %>% filter(Location %in% c("SR 167 at Kent (196th)","SR 512 at Pacific Ave. (SR 7)", "SR 410 at Sumner", "I-90 at West of Bandera")) %>% select(Location,Date, Ratio) %>% filter(Ratio <=2 & Ratio >=0.1)
other.chart <- create.line.chart(t=other.volumes, w.x='Date', w.y='Ratio', w.g='Location', w.yr='Date', 
                              w.title='', w.lab=label_percent(), w.x.title='', x.type = 'Date', d.form = "%B") 

# Retail Sales ------------------------------------------------------------
geodatabase.server <- "AWS-PROD-SQL\\Sockeye"
geodatabase.name <- "ElmerGeo"
gdb.nm <- paste0("MSSQL:server=",geodatabase.server,";database=",geodatabase.name,";trusted_connection=yes")
spn <- 2285
wgs84 <- 4326

city.lyr <- st_read(gdb.nm, "dbo.cities", crs = spn)
regeo <- city.lyr %>% st_drop_geometry() %>% select(city_name,class_desc)

sales.tax <- as_tibble(read.xlsx(sales.file, sheet = "TaxableRetailSales-Local", detectDates = TRUE, skipEmptyRows = TRUE, startRow = 10, colNames = TRUE))

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
  mutate(Geography = str_replace(Geography, "2021", "")) %>%
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

region.salestax.chart <- create_bar_chart(w.data = regional.sales.tax, w.x = "Quarter", w.y = "Total.Taxable", w.bartype = "stack", 
                                    w.transparent = 1.0, w.color = "Tax.Type")

tax.19 <- sales.tax %>% filter(Quarter =="2019-07-01", Tax.Type == "Sales") %>% rename(Q32019=Total.Taxable) %>% select("Geography","Q32019")
tax.21 <- sales.tax %>% filter(Quarter =="2021-07-01", Tax.Type == "Sales") %>% rename(Q32021=Total.Taxable) %>% select("Geography","Q32021")

tax.comparison <- left_join(tax.19, tax.21, by=c("Geography")) %>%
  mutate(Geography = str_replace(Geography, "/king", "")) %>%
  mutate(Geography = str_replace(Geography, "/pierce", "")) %>%
  mutate(Geography = str_replace(Geography, "/snohomish", "")) %>%
  group_by(Geography) %>%
  summarize_all(sum) %>%
  mutate(Geography = str_replace(Geography, " City", "")) %>%
  mutate(Geography = str_replace(Geography, "Beaux Arts Village", "Beaux Arts")) %>%
  mutate(Geography = str_replace(Geography, "Du Pont", "DuPont")) %>%
  mutate(Geography = str_replace(Geography, "Seatac", "Sea Tac")) %>%
  mutate(Ratio = Q32021/Q32019)

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


