library(data.table)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)
library(plotly)
library(openxlsx)
library(sf)

# Inputs ------------------------------------------------------------------
srv.dir <- 'X:/DSA/shiny-uploads/trends/chicago-title'

cont.claims.file <- file.path(srv.dir,'data','Continued Claims Demographics Published.37.xlsx')
nonhispanic.file <- file.path(srv.dir,'data','workers-by-race-quarter.xlsx')
hispanic.file <- file.path(srv.dir,'data','workers-by-hispanic-origin-race.xlsx')
gender.file <- file.path(srv.dir,'data','workers-by-gender.xlsx')
age.file <- file.path(srv.dir,'data','workers-by-age.xlsx')
edu.file <-file.path(srv.dir,'data','workers-by-education.xlsx')
init.claims.file <- file.path(srv.dir,'data', 'Initial Claims Demographics Week 9, 2020 to Week 37, 2021 Published.xlsx')
rentals.file <- file.path(srv.dir,'data', 'Metro_ZORI_AllHomesPlusMultifamily_SSA.csv')
home.sales.file <- file.path(srv.dir,'data', 'redfin-homesale-data.xlsx')

ofm.pop.file <- file.path(srv.dir,'data', 'ofm_april1_population_final.xlsx')
ofm.pop.chg.file <- file.path(srv.dir,'data', 'ofm_april1_components_of_change_1960_to_present.xlsx')
ofm.housing.file <- file.path(srv.dir,'data', 'ofm_april1_postcensal_estimates_housing_1980_1990-present.xlsx')

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
                 "Some College" = "#FBD6C9")

city.of.interest <- c("Seattle", "Bellevue", "Redmond")

county.of.interest <- c("King County", "Pierce County", "Snohomish County")

sectors.of.interest <- c("Manufacturing", "Leisure and Hospitality", "Construction", "Other Services")

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

# Airport Trends ----------------------------------------------------------
seatac.airport <- fread(file.path(srv.dir,'data','SEA activity measures by week - SEA measures.csv')) %>%
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

seatac.chart <- create.line.chart(t=seatac.airport, w.x='day', w.y='passenger_screenings', w.g='year', w.yr='year', 
                                  w.title='', w.lab=label_comma(), w.x.title='', x.type = 'Date', d.form = "%B")

latest.airport.wk <- seatac.airport %>% filter(year=="2021" & !(is.na(passenger_screenings))) %>% select(day) %>% unique() %>% pull() %>% max()
latest.seatac.screenings.2019 <- seatac.airport %>% filter(year=="2019" & day==latest.airport.wk) %>% select(passenger_screenings) %>% pull()
latest.seatac.screenings.2020 <- seatac.airport %>% filter(year=="2020" & day==latest.airport.wk) %>% select(passenger_screenings) %>% pull()
latest.seatac.screenings.2021 <- seatac.airport %>% filter(year=="2021" & day==latest.airport.wk) %>% select(passenger_screenings) %>% pull()
latest.seatac.screenings.ratio <- round(1 - (latest.seatac.screenings.2021/latest.seatac.screenings.2019),2)*100
latest.seatac.day <- day(ymd(latest.airport.wk))
latest.seatac.month <- month(ymd(latest.airport.wk), label=TRUE, abbr=FALSE)

minimum.seatac.screenings <- seatac.airport %>% filter(!(is.na(passenger_screenings))) %>% select(passenger_screenings) %>% pull() %>% min()
minimum.airport.wk <- seatac.airport %>% filter(passenger_screenings==minimum.seatac.screenings) %>% select(day) %>% pull()
minimum.seatac.day <- day(ymd(minimum.airport.wk))
minimum.seatac.month <- month(ymd(minimum.airport.wk), label=TRUE, abbr=FALSE)

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
  mutate(NAICS.INDUSTRY = factor(NAICS.INDUSTRY, levels = c("Manufacturing", "Construction", "Leisure and Hospitality", "Other Services")))

jobs.by.sector.chart <- create_facet_bar_chart(t=tbl, w.x='year', w.y='jobs', g='NAICS.INDUSTRY',w.title='Jobs', f='NAICS.INDUSTRY', w.facet=2)
rm(tbl)

latest.jobs.month <- month(ymd(qcew.jobs %>% select(month) %>% unique() %>% pull() %>% max()), label=TRUE, abbr=FALSE)

region.jobs.latest.mo.2019 <- qcew.jobs %>% filter(NAICS.INDUSTRY=="Total Nonfarm" & year==2019 & geography=="Region" & month(month)==max.month) %>% select(jobs) %>% pull()
region.jobs.latest.mo.2020 <- qcew.jobs %>% filter(NAICS.INDUSTRY=="Total Nonfarm" & year==2020 & geography=="Region" & month(month)==max.month) %>% select(jobs) %>% pull()
region.jobs.latest.mo.2021 <- qcew.jobs %>% filter(NAICS.INDUSTRY=="Total Nonfarm" & year==2021 & geography=="Region" & month(month)==max.month) %>% select(jobs) %>% pull()
state.jobs.latest.mo.2021 <- qcew.jobs %>% filter(NAICS.INDUSTRY=="Total Nonfarm" & year==2021 & geography=="Washington State" & month(month)==max.month) %>% select(jobs) %>% pull()

region.jobs.latest.mo.ratio.2020 <- round(region.jobs.latest.mo.2020/region.jobs.latest.mo.2019*100,0)
region.jobs.latest.mo.ratio.2021 <- round(region.jobs.latest.mo.2021/region.jobs.latest.mo.2019*100,0)
region.state.jobs.latest.mo.ratio.2021 <- round(region.jobs.latest.mo.2021/state.jobs.latest.mo.2021*100,0)

region.jobs.lowest.2020 <- qcew.jobs %>% filter(NAICS.INDUSTRY=="Total Nonfarm" & year==2020 & geography=="Region") %>% select(jobs) %>% pull() %>% min()
region.jobs.lowest.mo.2020 <- month(ymd(qcew.jobs %>% filter(NAICS.INDUSTRY=="Total Nonfarm" & year==2020 & geography=="Region" & jobs==region.jobs.lowest.2020) %>% select(month) %>% pull()), label=TRUE, abbr=FALSE)

region.jobs.change.similar.year.diff <- qcew.jobs %>% 
  filter(year > 2010 & year<2020 & geography=="Region" & NAICS.INDUSTRY=="Total Nonfarm") %>%
  mutate(CurrentYear = region.jobs.lowest.2020) %>%
  mutate(Diff = abs(jobs-CurrentYear)) %>%
  select(Diff) %>%
  pull() %>%
  min()

region.jobs.change.similar.date <- qcew.jobs %>% 
  filter(year > 2010 & year<2020 & geography=="Region" & NAICS.INDUSTRY=="Total Nonfarm") %>%
  mutate(CurrentYear = region.jobs.lowest.2020) %>%
  mutate(Diff = abs(jobs-CurrentYear)) %>%
  filter(Diff == region.jobs.change.similar.year.diff) %>%
  select(month) %>%
  pull()

region.jobs.change.similar.year.diff.2021 <- qcew.jobs %>% 
  filter(year > 2010 & year<2021 & geography=="Region" & NAICS.INDUSTRY=="Total Nonfarm") %>%
  mutate(CurrentYear = region.jobs.latest.mo.2021) %>%
  mutate(Diff = abs(jobs-CurrentYear)) %>%
  select(Diff) %>%
  pull() %>%
  min()

region.jobs.change.similar.date.2021 <- qcew.jobs %>% 
  filter(year > 2010 & year<2021 & geography=="Region" & NAICS.INDUSTRY=="Total Nonfarm") %>%
  mutate(CurrentYear = region.jobs.latest.mo.2021) %>%
  mutate(Diff = abs(jobs-CurrentYear)) %>%
  filter(Diff == region.jobs.change.similar.year.diff.2021) %>%
  select(month) %>%
  pull()

manufacturing.2019 <- qcew.jobs %>% 
  filter(year ==2019 & geography=="Region" & NAICS.INDUSTRY=="Manufacturing" & month(month)==max.month) %>%
  select(jobs) %>% pull()

manufacturing.2021 <- qcew.jobs %>% 
  filter(year ==2021 & geography=="Region" & NAICS.INDUSTRY=="Manufacturing" & month(month)==max.month) %>%
  select(jobs) %>% pull()

manufacturing.delta <- round(manufacturing.2021-manufacturing.2019,-2)
manufacturing.delta.percent <- round(((manufacturing.2021-manufacturing.2019)/manufacturing.2019)*100,0)

leisure.2019 <- qcew.jobs %>% 
  filter(year ==2019 & geography=="Region" & NAICS.INDUSTRY=="Leisure and Hospitality" & month(month)==max.month) %>%
  select(jobs) %>% pull()

leisure.2020 <- qcew.jobs %>% 
  filter(year ==2020 & geography=="Region" & NAICS.INDUSTRY=="Leisure and Hospitality" & month(month)==max.month) %>%
  select(jobs) %>% pull()

leisure.2021 <- qcew.jobs %>% 
  filter(year ==2021 & geography=="Region" & NAICS.INDUSTRY=="Leisure and Hospitality" & month(month)==max.month) %>%
  select(jobs) %>% pull()

leisure.delta.2020 <- round(leisure.2020-leisure.2019,-2)
leisure.delta.2021 <- round(leisure.2021-leisure.2019,-2)
leisure.delta.percent <- round(((leisure.2021-leisure.2019)/leisure.2019)*100,0)


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

tbl <- ofm.data %>%
  filter(Variable %in% c("Migration", "Natural Increase") & Jurisdiction == "Region" & Year >2010) %>%
  mutate(Year = as.character(Year))

region.pop.change.chart <- create_bar_chart(w.data = tbl, w.x = "Year", w.y = "Estimate", w.bartype = "stack", 
                                            w.transparent = 1.0, w.color = "Variable")

# City Changes
tbl <- ofm.data %>%
  filter(Variable=="Total Population" & Jurisdiction %in% city.of.interest & Year %in% c("2015","2018","2021")) %>%
  mutate(Year = as.character(Year)) %>%
  mutate(Jurisdiction = factor(Jurisdiction, levels = c("Bellevue","Redmond","Seattle")))

city.pop.change.chart <- create_facet_bar_chart(t=tbl, w.x='Year', w.y='Estimate', g='Jurisdiction',
                                                w.title='', f='Year', w.facet=3, w.scales="fixed", lab.clr="white", lab.sz=2)

# County Changes
tbl <- ofm.data %>%
  filter(Variable=="Total Population" & Jurisdiction %in% county.of.interest & Year %in% c("2015","2018","2021")) %>%
  mutate(Year = as.character(Year)) %>%
  mutate(Jurisdiction = factor(Jurisdiction, levels = c("King County", "Pierce County", "Snohomish County")))

county.pop.change.chart <- create_facet_bar_chart(t=tbl, w.x='Year', w.y='Estimate', g='Jurisdiction',
                                                w.title='', f='Year', w.facet=3, w.scales="fixed", lab.clr="white", lab.sz=2)

max.year <- ofm.data %>% select(Year) %>% unique() %>% pull() %>% max()

region.pop.change <- round(ofm.data %>% filter(Year==max.year & Jurisdiction=="Region" & Variable=="Migration") %>% select(Estimate) %>% pull() + 
  ofm.data %>% filter(Year==max.year & Jurisdiction=="Region" & Variable=="Natural Increase") %>% select(Estimate) %>% pull(),-2)

region.pop.change.migration <- round((ofm.data %>% filter(Year==max.year & Jurisdiction=="Region" & Variable=="Migration") %>% select(Estimate) %>% pull()) /  region.pop.change *100, 0)

region.pop.change.max <- ofm.data %>% 
  filter(Year > 2010 & Year!=2021 & Jurisdiction=="Region" & Variable == "Total Change") %>%
  select(Estimate) %>%  pull () %>%  max()

region.pop.change.max.year <- ofm.data %>% 
  filter(Year > 2010 & Year!=2021 & Jurisdiction=="Region" & Estimate == region.pop.change.max) %>%
  select(Year) %>%  pull ()

region.pop.change.max.migration <- round((ofm.data %>% 
  filter(Year == region.pop.change.max.year & Jurisdiction=="Region" & Variable == "Migration") %>%
  select(Estimate) %>%  pull ()) / region.pop.change.max *100, 0)

region.pop.change.max <- round(region.pop.change.max ,-2)

region.pop.change.similar.year.diff <- ofm.data %>% 
  filter(Year > 2010 & Year!=2021 & Jurisdiction=="Region" & Variable == "Total Change") %>%
  mutate(CurrentYear = region.pop.change) %>%
  mutate(Diff = abs(Estimate-CurrentYear)) %>%
  select(Diff) %>%
  pull() %>%
  min()

region.pop.change.similar.year <- ofm.data %>% 
  filter(Year > 2010 & Year!=2021 & Jurisdiction=="Region" & Variable == "Total Change") %>%
  mutate(CurrentYear = region.pop.change) %>%
  mutate(Diff = abs(Estimate-CurrentYear)) %>%
  filter(Diff == region.pop.change.similar.year.diff) %>%
  select(Year) %>%
  pull()

seattle.2015 <- ofm.data %>%
  filter(Year == 2015 & Jurisdiction == "Seattle" & Variable == "Total Population") %>%
  select(Estimate) %>% pull()

seattle.2021 <- ofm.data %>%
  filter(Year == 2021 & Jurisdiction == "Seattle" & Variable == "Total Population") %>%
  select(Estimate) %>% pull()

seattle.delta <- seattle.2021 - seattle.2015

bellevue.2015 <- ofm.data %>%
  filter(Year == 2015 & Jurisdiction == "Bellevue" & Variable == "Total Population") %>%
  select(Estimate) %>% pull()

bellevue.2021 <- ofm.data %>%
  filter(Year == 2021 & Jurisdiction == "Bellevue" & Variable == "Total Population") %>%
  select(Estimate) %>% pull()

bellevue.delta <- bellevue.2021 - bellevue.2015

redmond.2015 <- ofm.data %>%
  filter(Year == 2015 & Jurisdiction == "Redmond" & Variable == "Total Population") %>%
  select(Estimate) %>% pull()

redmond.2021 <- ofm.data %>%
  filter(Year == 2021 & Jurisdiction == "Redmond" & Variable == "Total Population") %>%
  select(Estimate) %>% pull()

redmond.delta <- redmond.2021 - redmond.2015

region.2015 <- ofm.data %>%
  filter(Year == 2015 & Jurisdiction == "Region" & Variable == "Total Population") %>%
  select(Estimate) %>% pull()

region.2021 <- ofm.data %>%
  filter(Year == 2021 & Jurisdiction == "Region" & Variable == "Total Population") %>%
  select(Estimate) %>% pull()

region.delta <- region.2021 - region.2015

king.2015 <- ofm.data %>%
  filter(Year == 2015 & Jurisdiction == "King County" & Variable == "Total Population") %>%
  select(Estimate) %>% pull()

king.2021 <- ofm.data %>%
  filter(Year == 2021 & Jurisdiction == "King County" & Variable == "Total Population") %>%
  select(Estimate) %>% pull()

king.delta <- king.2021 - king.2015
king.delta.percent <- round(king.delta/king.2015*100,0)
king.shr <- round(king.delta/region.delta*100,0)

kitsap.2015 <- ofm.data %>%
  filter(Year == 2015 & Jurisdiction == "Kitsap County" & Variable == "Total Population") %>%
  select(Estimate) %>% pull()

kitsap.2021 <- ofm.data %>%
  filter(Year == 2021 & Jurisdiction == "Kitsap County" & Variable == "Total Population") %>%
  select(Estimate) %>% pull()

kitsap.delta <- kitsap.2021 - kitsap.2015

pierce.2015 <- ofm.data %>%
  filter(Year == 2015 & Jurisdiction == "Pierce County" & Variable == "Total Population") %>%
  select(Estimate) %>% pull()

pierce.2021 <- ofm.data %>%
  filter(Year == 2021 & Jurisdiction == "Pierce County" & Variable == "Total Population") %>%
  select(Estimate) %>% pull()

pierce.delta <- pierce.2021 - pierce.2015
pierce.delta.percent <- round(pierce.delta/pierce.2015*100,0)

snohomish.2015 <- ofm.data %>%
  filter(Year == 2015 & Jurisdiction == "Snohomish County" & Variable == "Total Population") %>%
  select(Estimate) %>% pull()

snohomish.2021 <- ofm.data %>%
  filter(Year == 2021 & Jurisdiction == "Snohomish County" & Variable == "Total Population") %>%
  select(Estimate) %>% pull()

snohomish.delta <- snohomish.2021 - snohomish.2015
snohomish.delta.percent <- round(snohomish.delta/snohomish.2015*100,0)

seattle.shr <- round(seattle.delta / king.delta * 100,0)

brs.shr <- round((seattle.delta+bellevue.delta+redmond.delta)/ king.delta * 100, 0)

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
  filter(Variable=="Total Housing Units" & Jurisdiction %in% city.of.interest & Year %in% c("2015","2018","2021")) %>%
  mutate(Year = as.character(Year)) %>%
  mutate(Jurisdiction = factor(Jurisdiction, levels = c("Bellevue","Redmond","Seattle")))

city.hu.change.chart <- create_facet_bar_chart(t=tbl, w.x='Year', w.y='Estimate', g='Jurisdiction',
                                                w.title='', f='Year', w.facet=3, w.scales="fixed", lab.clr="white", lab.sz=2)

# City
tbl <- ofm.data %>%
  filter(Variable=="Total Housing Units" & Jurisdiction %in% county.of.interest & Year %in% c("2015","2018","2021")) %>%
  mutate(Year = as.character(Year)) %>%
  mutate(Jurisdiction = factor(Jurisdiction, levels = c("King County", "Pierce County", "Snohomish County")))

county.hu.change.chart <- create_facet_bar_chart(t=tbl, w.x='Year', w.y='Estimate', g='Jurisdiction',
                                               w.title='', f='Year', w.facet=3, w.scales="fixed", lab.clr="white", lab.sz=2)

hu.2015 <- ofm.data %>%
  filter(Year == 2015 & Jurisdiction == "Region" & Variable == "Total Housing Units") %>%
  select(Estimate) %>% pull()

hu.2020 <- ofm.data %>%
  filter(Year == 2020 & Jurisdiction == "Region" & Variable == "Total Housing Units") %>%
  select(Estimate) %>% pull()

hu.2021 <- ofm.data %>%
  filter(Year == 2021 & Jurisdiction == "Region" & Variable == "Total Housing Units") %>%
  select(Estimate) %>% pull()

hu.2021.sy <- round(hu.2021 - hu.2020,-2)
hu.2021.my <- round(hu.2021 - hu.2015,-2)

mf.hu.2015 <- ofm.data %>%
  filter(Year == 2015 & Jurisdiction == "Region" & Variable == "Two or More Housing Units") %>%
  select(Estimate) %>% pull()

mf.hu.2020 <- ofm.data %>%
  filter(Year == 2020 & Jurisdiction == "Region" & Variable == "Two or More Housing Units") %>%
  select(Estimate) %>% pull()

mf.hu.2021 <- ofm.data %>%
  filter(Year == 2021 & Jurisdiction == "Region" & Variable == "Two or More Housing Units") %>%
  select(Estimate) %>% pull()

mf.hu.2021.sy <- round(mf.hu.2021 - mf.hu.2020,-2)
mf.hu.2021.my <- round(mf.hu.2021 - mf.hu.2015,-2)

mf.shr.sy <- round(mf.hu.2021.sy/hu.2021.sy *100,0)
mf.shr.my <- round(mf.hu.2021.my/hu.2021.my *100,0)

seattle.hu.2015 <- ofm.data %>%
  filter(Year == 2015 & Jurisdiction == "Seattle" & Variable == "Total Housing Units") %>%
  select(Estimate) %>% pull()

seattle.hu.2021 <- ofm.data %>%
  filter(Year == 2021 & Jurisdiction == "Seattle" & Variable == "Total Housing Units") %>%
  select(Estimate) %>% pull()

seattle.hu.delta <- seattle.hu.2021 - seattle.hu.2015

bellevue.hu.2015 <- ofm.data %>%
  filter(Year == 2015 & Jurisdiction == "Bellevue" & Variable == "Total Housing Units") %>%
  select(Estimate) %>% pull()

bellevue.hu.2021 <- ofm.data %>%
  filter(Year == 2021 & Jurisdiction == "Bellevue" & Variable == "Total Housing Units") %>%
  select(Estimate) %>% pull()

bellevue.hu.delta <- bellevue.hu.2021 - bellevue.hu.2015

redmond.hu.2015 <- ofm.data %>%
  filter(Year == 2015 & Jurisdiction == "Redmond" & Variable == "Total Housing Units") %>%
  select(Estimate) %>% pull()

redmond.hu.2021 <- ofm.data %>%
  filter(Year == 2021 & Jurisdiction == "Redmond" & Variable == "Total Housing Units") %>%
  select(Estimate) %>% pull()

redmond.hu.delta <- redmond.hu.2021 - redmond.hu.2015

king.hu.2015 <- ofm.data %>%
  filter(Year == 2015 & Jurisdiction == "King County" & Variable == "Total Housing Units") %>%
  select(Estimate) %>% pull()

king.hu.2021 <- ofm.data %>%
  filter(Year == 2021 & Jurisdiction == "King County" & Variable == "Total Housing Units") %>%
  select(Estimate) %>% pull()

king.hu.delta <- king.hu.2021 - king.hu.2015
king.hu.delta.percent <- round(king.hu.delta/king.hu.2015*100,0)
king.hu.shr <- round(king.hu.delta/hu.2021.my*100,0)

kitsap.hu.2015 <- ofm.data %>%
  filter(Year == 2015 & Jurisdiction == "Kitsap County" & Variable == "Total Housing Units") %>%
  select(Estimate) %>% pull()

kitsap.hu.2021 <- ofm.data %>%
  filter(Year == 2021 & Jurisdiction == "Kitsap County" & Variable == "Total Housing Units") %>%
  select(Estimate) %>% pull()

kitsap.hu.delta <- kitsap.hu.2021 - kitsap.hu.2015
kitsap.hu.delta.percent <- round(kitsap.hu.delta/kitsap.hu.2015*100,0)

pierce.hu.2015 <- ofm.data %>%
  filter(Year == 2015 & Jurisdiction == "Pierce County" & Variable == "Total Housing Units") %>%
  select(Estimate) %>% pull()

pierce.hu.2021 <- ofm.data %>%
  filter(Year == 2021 & Jurisdiction == "Pierce County" & Variable == "Total Housing Units") %>%
  select(Estimate) %>% pull()

pierce.hu.delta <- pierce.hu.2021 - pierce.hu.2015
pierce.hu.delta.percent <- round(pierce.hu.delta/pierce.hu.2015*100,0)

snohomish.hu.2015 <- ofm.data %>%
  filter(Year == 2015 & Jurisdiction == "Snohomish County" & Variable == "Total Housing Units") %>%
  select(Estimate) %>% pull()

snohomish.hu.2021 <- ofm.data %>%
  filter(Year == 2021 & Jurisdiction == "Snohomish County" & Variable == "Total Housing Units") %>%
  select(Estimate) %>% pull()

snohomish.hu.delta <- snohomish.hu.2021 - snohomish.hu.2015
snohomish.hu.delta.percent <- round(snohomish.hu.delta/snohomish.hu.2015*100,0)

seattle.hu.shr <- round(seattle.hu.delta / king.hu.delta * 100,0)

brs.hu.shr <- round((seattle.hu.delta+bellevue.hu.delta+redmond.hu.delta)/ king.hu.delta * 100, 0)

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
