library(data.table)
library(here)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(plotly)
library(openxlsx)
library(sf)

# Inputs ------------------------------------------------------------------
cont.claims.file <- here('data','Continued Claims Demographics Published.37.xlsx')
nonhispanic.file <- here('data','workers-by-race-quarter.xlsx')
hispanic.file <- here('data','workers-by-hispanic-origin-race.xlsx')
gender.file <- here('data','workers-by-gender.xlsx')
age.file <- here('data','workers-by-age.xlsx')
edu.file <- here('data','workers-by-education.xlsx')
init.claims.file <- here('data', 'Initial Claims Demographics Week 9, 2020 to Week 37, 2021 Published.xlsx')
rentals.file <- here('data', 'Metro_ZORI_AllHomesPlusMultifamily_SSA.csv')
home.sales.file <- here('data', 'redfin-homesale-data.xlsx')

ofm.pop.file <- here('data', 'ofm_april1_population_final.xlsx')
ofm.pop.chg.file <- here('data', 'ofm_april1_components_of_change_1960_to_present.xlsx')
ofm.housing.file <- here('data', 'ofm_april1_postcensal_estimates_housing_1980_1990-present.xlsx')

congestion.times <- c("5AM", "7AM", "9AM", "11AM", "1PM", "3PM", "5PM","7PM")

education.lookup <- c("Did not finish high school" = "Less than high school",
                "High School Diploma, including GED" = "High school or equivalent, no college",
                "Associate's Degree" = "Some college or Associate degree",
                "Some College" = "Some college or Associate degree",
                "Bachelor's Degree" = "Bachelor's degree or advanced degree",
                "Master's Degree" = "Bachelor's degree or advanced degree",
                "PhD" = "Bachelor's degree or advanced degree",
                "Post-Baccalaureate Degree" = "Bachelor's degree or advanced degree",
                "No Schooling" = "Educational attainment not available (workers aged 24 or younger)",
                "Unknown" = "Educational attainment not available (workers aged 24 or younger)")

education.lookup <- enframe(education.lookup)

psrc.counties <- c("King","Kitsap","Pierce","Snohomish")

# Spatial Layers ----------------------------------------------------------
geodatabase.server <- "AWS-PROD-SQL\\Sockeye"
geodatabase.name <- "ElmerGeo"
gdb.nm <- paste0("MSSQL:server=",geodatabase.server,";database=",geodatabase.name,";trusted_connection=yes")
spn <- 2285
wgs84 <- 4326

city.lyr <- st_read(gdb.nm, "dbo.cities", crs = spn)

regeo <- city.lyr %>% 
  st_drop_geometry() %>% 
  select(city_name,cnty_name,class_desc) %>%
  mutate(County = case_when(
    city_name == "Auburn" ~ "King & Pierce",
    city_name == "Bothell" ~ "King & Snohomish",
    city_name == "Pacific" ~ "King & Pierce",
    city_name == "Enumclaw" ~ "King & Pierce",
    city_name == "Milton" ~ "King & Pierce")) %>%
  mutate(County = case_when(
    is.na(County) ~ cnty_name,
    !(is.na(County)) ~ County)) %>%
  select(-cnty_name) %>%
  distinct() %>%
  mutate(city_name = gsub("Beaux Arts", "Beaux Arts Village", city_name)) %>%
  mutate(city_name = gsub("Sea Tac", "SeaTac", city_name))

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

create_facet_bar_chart <- function(t, w.x, w.y, g, w.scales="free", w.facet=3, d=0, w.factor=1, s="", w.dec = 0, w.title, f, w.label=label_comma(), w.colors=psrc.colors, w.pos="dodge") {
  
  c <- ggplot(data=t, 
              aes(y=get(eval(w.y)), 
                  x=get(eval(w.x)), 
                  fill = get(eval(f)),
              ))+
    geom_bar(position=w.pos, stat="identity") +
    geom_text(aes(label = paste0(prettyNum(round(get(eval(w.y))*w.factor,w.dec), big.mark = ","),s)), vjust = 1.5, colour = "white", size=3) +
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

# Congestion Trends -------------------------------------------------------
data.years <- seq(2017,2021,1)
tmc.columns <- c('Tmc','road','direction','county','miles','thrulanes','aadt',
                 '5am_ratio','6am_ratio','7am_ratio','8am_ratio','9am_ratio','10am_ratio','11am_ratio','Noon_ratio',
                 '1pm_ratio','2pm_ratio','3pm_ratio','4pm_ratio','5pm_ratio','6pm_ratio','7pm_ratio','8pm_ratio')

moderate.ratio <- 0.75
heavy.ratio <- 0.50
severe.ratio <- 0.25

psrc.colors <- c("2019" = "#91268F",
                 "2020" = "#F05A28",
                 "2021" = "#8CC63E",
                 "Moderate" = "orange",
                 "Heavy" = "red",
                 "Severe" = "black",
                 "Region" = "#91268F",
                 "Washington State" = "#4C4C4C",
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
                 "Bellevue" = "#630460",
                 "Bremerton" = "#00A7A0",
                 "Everett" = "#F05A28",
                 "Seattle" = "#8CC63E",
                 "Tacoma" = "#AD5CAB",
                 "Auburn" = "#91268F",
                 "Bothell" = "#AD5CAB",
                 "Burien" = "#C388C2",
                 "Federal Way" = "#F05A28",
                 "Issaquah" = "#F4835E",
                 "Kent" = "#F7A489",
                 "Kirkland" = "#8CC63E",
                 "Lakewood" = "#A9D46E",
                 "Lynnwood" = "#C0E095",
                 "Puyallup" = "#00A7A0",
                 "Redmond" = "#40BDB8",
                 "Renton" = "#73CFCB",
                 "SeaTac" = "#4C4C4C",
                 "Tukwila" = "#76787A",
                 "University Place" = "#999999",
                 "Arlington" = "#630460",
                 "Bainbridge Island" = "#91268F",
                 "Des Moines" = "#AD5CAB",
                 "DuPont" = "#C388C2",
                 "Edmonds" = "#9f3913",
                 "Fife" = "#F05A28",
                 "Fircrest" = "#F4835E",
                 "Kenmore" = "#F7A489",
                 "Lake Forest Park" = "#588527",
                 "Marysville" = "#8CC63E",
                 "Mercer Island" = "#A9D46E",
                 "Mill Creek" = "#C0E095",
                 "Mountlake Terrace" = "#00716c",
                 "Mukilteo" = "#00A7A0",
                 "Newcastle" = "#40BDB8",
                 "Port Orchard" = "#73CFCB",
                 "Poulsbo" = "#3e4040",
                 "Shoreline" = "#4C4C4C",
                 "Sumner" = "#76787A",
                 "Woodinville" = "#999999",
                 "Single-Family" = "#F05A28",
                 "Multi-Family" = "#8CC63E",
                 "Mobile-Home" = "#91268F",
                 "Metro" = "#8CC63E",
                 "Core" = "#00A7A0",
                 "HCT" = "#F05A28",
                 "CitiesTowns" = "#91268F",
                 "Unincorporated" = "#4C4C4C",
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
                 "Bachelor's degree or advanced degree" = "#9f3913",
                 "Educational attainment not available (workers aged 24 or younger)" = "#F05A28",
                 "High school or equivalent, no college" = "#F4835E",
                 "Less than high school" = "#F7A489",
                 "Some college or Associate degree" = "#FBD6C9")

tmc.data <- NULL
for (yrs in data.years){
  
  t <- fread(here('data',paste0('May_',yrs,'_Cars_tmc_95th_percentile_speed.csv'))) %>%
    select(all_of(tmc.columns)) %>%
    mutate(across(everything(), ~replace_na(.x, 1))) %>%
    mutate(year=yrs)
  
  t <- t %>%
    pivot_longer(cols=contains("_ratio"), names_to = "tod", values_to ="ratio") %>%
    mutate(tod=gsub("_ratio","",tod)) %>%
    mutate(tod=gsub("Noon","12pm",tod)) %>%
    mutate(tod=gsub("am",":00am",tod), tod=gsub("pm",":00pm",tod)) %>%
    mutate(ratio = round(ratio,2)) %>%
    mutate(tod = format(as.POSIXct(tod,format='%I:%M %p'),format="%H:%M:%S")) %>%
    mutate(tod = hms(tod)) %>%
    mutate(congestion_level = case_when(
      ratio <= severe.ratio ~ 'Severe',
      (ratio > severe.ratio & ratio <= heavy.ratio) ~ 'Heavy',
      (ratio > heavy.ratio & ratio <= moderate.ratio) ~ 'Moderate',
      ratio > moderate.ratio ~ 'Minimal'))%>%
    mutate(congestion = case_when(
      ratio <= heavy.ratio ~ 'Heavy Congestion',
      ratio > heavy.ratio ~ 'Moderate Congestion'))

  ifelse(is.null(tmc.data), tmc.data <- t, tmc.data <- bind_rows(tmc.data,t))
  
  rm(t)
  
}

tmc.data <- tmc.data %>% mutate(lane_miles=miles*thrulanes)

congestion.level.tod <- tmc.data %>%
  group_by(year, tod, congestion_level) %>%
  summarize(congested_lane_miles = round(sum(lane_miles),0))

congestion.tod <- tmc.data %>%
  group_by(year, tod, congestion) %>%
  summarize(congested_lane_miles = round(sum(lane_miles),0))

t <- tmc.data %>%
  group_by(year, tod) %>%
  summarize(total_lane_miles = round(sum(lane_miles),0))

congestion.level.tod <- left_join(congestion.level.tod, t, by=c("year","tod")) %>%
  mutate(percent_congested = round(congested_lane_miles / total_lane_miles,2))

congestion.tod <- left_join(congestion.tod, t, by=c("year","tod")) %>%
  mutate(percent_congested = round(congested_lane_miles / total_lane_miles,2))

rm(t)  

severe <- congestion.level.tod %>% 
  filter(congestion_level=="Severe", year >= 2019) %>% 
  mutate(hr = case_when(
    hour(tod) <12 ~ paste0(hour(tod),"AM"),
    hour(tod) ==12 ~ "12PM",
    hour(tod) >12 ~ paste0(hour(tod)-12,"PM"))) %>%
  select(year, tod, hr, percent_congested) %>%
  mutate(year = as.character(year)) %>%
  mutate(hr = factor(hr, levels = c("5AM","6AM","7AM","8AM","9AM","10AM","11AM","12PM",
                                    "1PM","2PM","3PM","4PM","5PM","6PM","7PM","8PM")))

severe.congestion.chart <- create.line.chart(t=severe, w.x='hr', w.y='percent_congested', w.g='year', w.yr='year', 
                                             w.title='', f=100, s="%", w.lab=label_percent(), w.x.title='', w.breaks=congestion.times)

# Airport Trends ----------------------------------------------------------
seatac.airport <- fread(here('data','SEA activity measures by week - SEA measures.csv')) %>%
  select(1:4) %>%
  slice(2:n()) %>%
  setnames(c("Week","2021","2020","2019")) %>%
  mutate(Week = gsub("Week ","",Week)) %>%
  mutate(Week  =as.integer(Week)) %>%
  pivot_longer(cols=c("2021","2020","2019"), names_to = "year", values_to ="passenger_screenings") %>%
  mutate(day = (ymd( "2021-01-01" ) + weeks(Week-1))) %>%
  mutate(passenger_screenings = gsub(",","",passenger_screenings)) %>%
  mutate(passenger_screenings  =as.integer(passenger_screenings)) %>%
  mutate(year =as.character(year))

national.airport <- fread(here('data','national-tsa-daily-screenings.csv')) %>%
  mutate(Date = mdy(Date)) %>%
  pivot_longer(cols=contains("Traveler"), names_to = "year", values_to ="passenger_screenings") %>%
  mutate(year= gsub(" Traveler Throughput","",year)) %>%
  mutate(year  =as.integer(year)) %>%
  mutate(passenger_screenings = gsub(",","",passenger_screenings)) %>%
  mutate(passenger_screenings  =as.integer(passenger_screenings)) %>%
  mutate(Week=week(Date)) %>%
  mutate(Week =as.integer(Week)) %>%
  mutate(year =as.character(year)) %>%
  group_by(year,Week) %>%
  summarize(passenger_screenings = median(passenger_screenings))

t <- seatac.airport %>% select(Week, year, day)
national.airport <- left_join(national.airport,t,by=c("Week","year"))
rm(t)

seatac.chart <- create.line.chart(t=seatac.airport, w.x='day', w.y='passenger_screenings', w.g='year', w.yr='year', 
                                  w.title='', w.lab=label_comma(), w.x.title='', x.type = 'Date', d.form = "%B")
national.airports.chart <- create.line.chart(t=national.airport, w.x='day', w.y='passenger_screenings', w.g='year', 
                                             w.yr='year', w.title='', w.lab=label_comma(), w.x.title='', x.type = 'Date', d.form = "%B")

# Employment Trends -------------------------------------------------------
qcew.areas <- c("Washington State", "Seattle MSA", "Tacoma MSA", "Bremerton MSA")

goods.producing <- c("Mining and Logging", "Construction", "Manufacturing")
private.services <- c("Trade, Transportation, and Utilities", "Information", "Financial Activities", "Professional and Business Services",
                      "Educational and Health Services", "Leisure and Hospitality", "Other Services")
government.services <- c("Government")
job.categories <- c(goods.producing, private.services,government.services)

qcew.jobs<- NULL
for (areas in qcew.areas) {

  t <- as_tibble(read.xlsx(here('data','WA-QB-historical-SA.xlsx'), sheet = areas, detectDates = TRUE, skipEmptyRows = TRUE, startRow = 2, colNames = TRUE)) %>%
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
  filter(NAICS.INDUSTRY%in%c(job.categories,"Total Nonfarm")) %>% 
  filter(geography %in% c("Region")) %>% 
  filter(year(month)>=2018)%>% filter(month(month)==max.month) %>%
  mutate(NAICS.INDUSTRY = factor(NAICS.INDUSTRY, levels = c("Manufacturing", "Construction", "Mining and Logging",
                                                            "Leisure and Hospitality", "Trade, Transportation, and Utilities", "Other Services",
                                                            "Financial Activities", "Information", "Professional and Business Services",
                                                            "Government", "Educational and Health Services", "Total Nonfarm")))

jobs.by.sector.chart <- create_facet_bar_chart(t=tbl, w.x='year', w.y='jobs', g='NAICS.INDUSTRY',w.title='Jobs', f='NAICS.INDUSTRY')
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

# Unemployment Trends by Race ------------------------------------------------------------
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

region.continuing.claims <- left_join(region.continuing.claims, workforce.by.race, by=c("Race")) %>%
  mutate(Share = (Estimate/WorkForce)) %>%
  filter(Race!="Total") %>%
  mutate(year = year(Date)) %>%
  mutate(year = as.character(year))

ue.claims.race.chart <- create.line.chart(t=region.continuing.claims, w.x='Date', w.y='Share', w.g='Race', 
                                          w.yr='year', w.title='',w.lab=label_percent(), w.x.title='', x.type = 'Date', d.form="%b-%Y")

# Unemployment Trends by Gender ------------------------------------------------------------
king.cont.claims <- as_tibble(read.xlsx(cont.claims.file, sheet = "King", detectDates = TRUE, skipEmptyRows = TRUE, startRow = 6, colNames = TRUE)) %>% slice(1:4) %>% mutate_at(vars(contains('Week')), as.numeric) %>% mutate(County="King")
kitsap.cont.claims <- as_tibble(read.xlsx(cont.claims.file, sheet = "Kitsap", detectDates = TRUE, skipEmptyRows = TRUE, startRow = 6, colNames = TRUE)) %>% slice(1:4) %>% mutate_at(vars(contains('Week')), as.numeric) %>% mutate(County="Kitsap")
pierce.cont.claims <- as_tibble(read.xlsx(cont.claims.file, sheet = "Pierce", detectDates = TRUE, skipEmptyRows = TRUE, startRow = 6, colNames = TRUE)) %>% slice(1:4) %>% mutate_at(vars(contains('Week')), as.numeric) %>% mutate(County="Pierce")
snohomish.cont.claims <- as_tibble(read.xlsx(cont.claims.file, sheet = "Snohomish", detectDates = TRUE, skipEmptyRows = TRUE, startRow = 6, colNames = TRUE)) %>% slice(1:4) %>% mutate_at(vars(contains('Week')), as.numeric) %>% mutate(County="Snohomish")

continuing.claims <- bind_rows(list(king.cont.claims,kitsap.cont.claims,pierce.cont.claims,snohomish.cont.claims))

continuing.claims <- continuing.claims %>% 
  select(-Demographic) %>% rename(Gender=Industry) %>% 
  pivot_longer(cols= contains("Week"), names_to="Date", values_to="Estimate") %>%
  mutate(Date = gsub(".*ending.","",Date), Date = gsub("\\."," ",Date)) %>%
  mutate(Date = mdy(Date)) %>%
  mutate(Gender = gsub(", All Unduplicated Claimants","",Gender), Gender = trimws(Gender, "both"))

region.continuing.claims.gender <- continuing.claims %>%
  select(-County) %>%
  group_by(Gender,Date) %>%
  summarize_all(sum) %>%
  mutate(Gender = gsub("Nonbinary/Other","Nonbinary",Gender))

region.continuing.claims.gender <- left_join(region.continuing.claims.gender, workforce.by.gender, by=c("Gender")) %>%
  mutate(Share = (Estimate/WorkForce)) %>%
  filter(Gender!="Total") %>%
  mutate(year = year(Date)) %>%
  mutate(year = as.character(year)) %>%
  filter(Gender!="Nonbinary")

ue.claims.gender.chart <- create.line.chart(t=region.continuing.claims.gender, w.x='Date', w.y='Share', w.g='Gender', 
                                          w.yr='year', w.title='',w.lab=label_percent(), w.x.title='', x.type = 'Date', d.form="%b-%Y")

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
  mutate(demographic=str_wrap(demographic,width=10))

initial.claims.chart <- create_facet_bar_chart(t=initial.claims, w.x='demographic', w.y='initial_claims', g='category',
                                               w.title='Initial Claims', f='demographic', w.facet=2)

initial.claims.share.chart <- create_facet_bar_chart(t=initial.claims, w.x='demographic', w.y='share', g='category',
                                               w.title='Initial Claims', f='demographic', w.facet=2, w.factor = 100, s="%")

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

# Create Region Total
region.housing <- ofm.housing %>%
  filter(Filter == "1") %>%
  group_by(Year,Variable) %>%
  summarize(Estimate=sum(Estimate)) %>%
  mutate(Filter="5", Jurisdiction="Region")

ofm.housing <- bind_rows(ofm.housing, region.housing) %>% mutate(Filter=as.numeric(Filter))

ofm.data <- bind_rows(ofm.pop, ofm.housing)
ofm.data <- left_join(ofm.data,regeo, by=c("Jurisdiction" = "city_name"))

tbl <- ofm.data %>% 
  filter(Variable=="Total Population" & Filter==1) %>%
  mutate(Jurisdiction = factor(Jurisdiction, levels = c("King County", "Kitsap County", "Pierce County", "Snohomish County", "Region")))
annual.county.pop.chart <- create.line.chart(t=tbl, w.x='Year', w.y='Estimate', w.g='Jurisdiction', 
                                            w.yr='Year', w.title='',w.lab=label_comma(), w.x.title='')
rm(tbl)

tbl <- ofm.data %>%
  filter(Variable %in% c("Migration", "Natural Increase") & Jurisdiction == "Region" & Year >=2010)
region.pop.change.chart <- create_bar_chart(w.data = tbl, w.x = "Year", w.y = "Estimate", w.bartype = "stack", 
                                            w.transparent = 1.0, w.color = "Variable")
rm(tbl)

tbl <- ofm.data %>%
  filter(Year >= 2010 & Variable == "Total Population") %>%
  mutate(class_desc = case_when(
    str_detect(Jurisdiction, "Unincorporated") ~ "Unincorporated",
    !(str_detect(Jurisdiction, "Unincorporated")) ~ class_desc)) %>%
  mutate(class_desc = case_when(
    str_detect(Jurisdiction, "Region") ~ "Region",
    !(str_detect(Jurisdiction, "Region")) ~ class_desc)) %>%
  filter(!(is.na(class_desc))) %>%
  filter(class_desc != "Region", Filter<100) %>%
  select(Year, class_desc, Estimate) %>%
  group_by(Year, class_desc) %>%
  summarize(Estimate = sum(Estimate)) %>%
  arrange(class_desc,Year)

tbl <-as_tibble(tbl) %>%
  filter(Year == 2010 | Year == 2021) %>%
  mutate(Delta = Estimate - lag(Estimate)) %>%
  filter(Year == 2021) %>%
  mutate(Year = as.character(Year)) %>%
  mutate(class_desc = factor(class_desc, levels = c("Metro", "Core", "HCT", "CitiesTowns", "Unincorporated")))

pop.rgeo.chart <- create_bar_chart(w.data = tbl, w.x = "class_desc", w.y = "Delta", w.bartype = "dodge", 
                                  w.transparent = 1.0, w.color = "class_desc")

tbl <- ofm.data %>%
  filter(Variable=="Total Population" & Year %in% c(2010,2015,2021) & Jurisdiction %in% c("Bellevue","Bremerton","Everett", "Seattle", "Tacoma")) %>%
  mutate(Year = as.character(Year))
metro.city.pop.chart <- create_facet_bar_chart(t=tbl, w.x='Year', w.y='Estimate', g='Jurisdiction',w.title='', f='Jurisdiction')
rm(tbl)

tbl <- ofm.data %>% filter(Variable=="Total Population" & Filter==4 & Year %in% c(2015,2021) & class_desc=="Core") %>% mutate(Year = as.character(Year))
core.city.pop.chart <- create_facet_bar_chart(t=tbl, w.x='Year', w.y='Estimate', g='Jurisdiction',w.title='', f='Jurisdiction')
rm(tbl)

tbl <- ofm.data %>% filter(Variable=="Total Population" & Filter==4 & Year %in% c(2015,2021) & class_desc=="HCT") %>% mutate(Year = as.character(Year))
hct.city.pop.chart <- create_facet_bar_chart(t=tbl, w.x='Year', w.y='Estimate', g='Jurisdiction',w.title='', f='Jurisdiction', w.facet=4)
rm(tbl)

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

tbl <- ofm.data %>%
  filter(Year >= 2010 & Variable == "Total Housing Units") %>%
  mutate(class_desc = case_when(
     str_detect(Jurisdiction, "Unincorporated") ~ "Unincorporated",
     !(str_detect(Jurisdiction, "Unincorporated")) ~ class_desc)) %>%
  mutate(class_desc = case_when(
    str_detect(Jurisdiction, "Region") ~ "Region",
    !(str_detect(Jurisdiction, "Region")) ~ class_desc)) %>%
  filter(!(is.na(class_desc))) %>%
  select(Year, class_desc, Estimate) %>%
  group_by(Year, class_desc) %>%
  summarize(Estimate = sum(Estimate)) %>%
  arrange(class_desc,Year)

tbl <-as_tibble(tbl) %>%
  filter(Year == 2010 | Year == 2021) %>%
  filter(class_desc != "Region") %>%
  mutate(Delta = Estimate - lag(Estimate)) %>%
  filter(Year == 2021) %>%
  mutate(Year = as.character(Year)) %>%
  mutate(class_desc = factor(class_desc, levels = c("Metro", "Core", "HCT", "CitiesTowns", "Unincorporated")))

hu.rgeo.chart <- create_bar_chart(w.data = tbl, w.x = "class_desc", w.y = "Delta", w.bartype = "dodge", 
                                  w.transparent = 1.0, w.color = "class_desc")

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
