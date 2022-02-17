library(tidyverse)

get_ofm_postcensal_population <- function (ofm.url="https://ofm.wa.gov/sites/default/files/public/dataresearch/pop/april1/hseries/ofm_april1_postcensal_estimates_pop_1960-present.xlsx", counties = c("King","Kitsap","Pierce","Snohomish")) {
  
  download.file(ofm.url, "working.xlsx", quiet = TRUE, mode = "wb")
  ofm.pop.file <- paste0(getwd(),"/working.xlsx")
  ofm <- readxl::read_excel(ofm.pop.file, sheet="Population",skip=3)
  file.remove(ofm.pop.file)
  
  ofm <- ofm %>% 
    dplyr::filter(County %in% counties) %>%
    dplyr::select(!(dplyr::contains("."))) %>%
    dplyr::select(-Line) %>%
    tidyr::pivot_longer(cols=dplyr::contains("Population"), names_to="Variable", values_to="Estimate") %>%
    dplyr::filter(Estimate != ".") %>%
    dplyr::mutate(Estimate = as.numeric(Estimate)) %>%
    dplyr::mutate(Variable = gsub("\r\n","",Variable)) %>%
    dplyr::mutate(Year = stringr::str_sub(Variable, 1, 4)) %>%
    dplyr::mutate(Year = as.numeric(Year)) %>%
    dplyr::filter(!(Variable %in% c("1970 Postcensal Estimate of Total Population","1980 Postcensal Estimate of Total Population",
                                    "1990 Postcensal Estimate of Total Population","2000 Postcensal Estimate of Total Population",
                                    "2010 Postcensal Estimate of Total Population","2020 Postcensal Estimate of Total Population"))) %>%
    dplyr::mutate(Variable = stringr::str_sub(Variable, 6, stringr::str_length(Variable)))
  
  region <- ofm %>%
    dplyr::filter(Filter <= 3) %>%
    dplyr::select(Filter, Variable, Year, Estimate) %>%
    dplyr::group_by(Filter, Variable, Year) %>%
    dplyr::summarize_all(sum) %>%
    dplyr::mutate(County = "Region") %>%
    dplyr::mutate(Jurisdiction = "Region") %>%
    dplyr::mutate(Jurisdiction = ifelse(Filter == 2, "Unincorporated Region", Jurisdiction)) %>%
    dplyr::mutate(Jurisdiction = ifelse(Filter == 3, "Incorporated Region", Jurisdiction))
  
  ofm <- dplyr::bind_rows(ofm,region) %>% dplyr::mutate(Variable = "Population", Data_Type = "Postcensal Estimates") 
  
  return(ofm)
  
}  

get_ofm_housing_units <- function (ofm.url="https://ofm.wa.gov/sites/default/files/public/dataresearch/pop/april1/hseries/ofm_april1_postcensal_estimates_housing_1980_1990-present.xlsx", counties = c("King","Kitsap","Pierce","Snohomish")) {

  download.file(ofm.url, "working.xlsx", quiet = TRUE, mode = "wb")
  ofm.file <- paste0(getwd(),"/working.xlsx")
  ofm <- readxl::read_excel(ofm.file, sheet="Housing Units",skip=3)
  file.remove(ofm.file)
  
  ofm <- ofm %>% 
    dplyr::filter(County %in% counties) %>%
    dplyr::select(-Line) %>%
    tidyr::pivot_longer(cols=dplyr::contains("Housing Units"), names_to="Variable", values_to="Estimate") %>%
    dplyr::filter(Estimate != ".") %>%
    dplyr::mutate(Estimate = as.numeric(Estimate)) %>%
    dplyr::mutate(Variable = gsub("\r\n","",Variable)) %>%
    dplyr::mutate(Year = stringr::str_sub(Variable, 1, 4)) %>%
    dplyr::mutate(Year = as.numeric(Year)) %>%
    dplyr::mutate(Variable = stringr::str_sub(Variable, 5, stringr::str_length(Variable))) %>%
    dplyr::mutate(Variable = stringr::str_trim(Variable, side = c("both"))) %>%
    dplyr::mutate(Variable = dplyr::case_when(
      stringr::str_detect(Variable, "Mobile Home") ~ "Mobile Home Units",
      stringr::str_detect(Variable, "One Unit Housing Units") ~ "Single-Family Units",
      stringr::str_detect(Variable, "Two or More Housing Units") ~ "Multi-Family Units",
      stringr::str_detect(Variable, "Total Housing Units") ~ "Total Housing Units")) 
  
  region <- ofm %>%
    dplyr::filter(Filter <= 3) %>%
    dplyr::select(Filter, Variable, Year, Estimate) %>%
    dplyr::group_by(Filter, Variable, Year) %>%
    dplyr::summarize_all(sum) %>%
    dplyr::mutate(County = "Region") %>%
    dplyr::mutate(Jurisdiction = "Region") %>%
    dplyr::mutate(Jurisdiction = ifelse(Filter == 2, "Unincorporated Region", Jurisdiction)) %>%
    dplyr::mutate(Jurisdiction = ifelse(Filter == 3, "Incorporated Region", Jurisdiction))
  
  ofm <- dplyr::bind_rows(ofm,region) %>% dplyr::mutate(Data_Type = "Postcensal Estimates") 
    
 return(ofm) 
  
}  
  
pop.data <- get_ofm_postcensal_population()
housing.data <- get_ofm_housing_units()





