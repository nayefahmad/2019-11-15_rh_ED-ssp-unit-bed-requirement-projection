
#'--- 
#' title: "RHS ED - Short Stay Pediatric (SSP) unit bed projections "
#' author: "Nayef Ahmad"
#' date: "2019-11-25"
#' output: 
#'   html_document: 
#'     keep_md: yes
#'     code_folding: show
#'     toc: true
#'     toc_float:
#'       collapsed: false
#'     toc_folding: false
#' ---
#' 

#+ lib, include = FALSE
library(tidyverse)
library(denodoExtractor)
library(DT)
library(lubridate)

setup_sql_server()

cnx <- DBI::dbConnect(odbc::odbc(), dsn = "cnx_SPDBSCSTA001")
vw_census <- dplyr::tbl(cnx, dbplyr::in_schema("[ADTCMart].[ADTC]", 
                                               "[CensusView]"))


#+ rest 

#' # Overview 
#' 
#' 



#' # Identifying SSP patients 
#' In ED: Identify SSP patients using `LastEmergencyAreaDescription` = `Shortstay Peds - ED` 
#' 
#' Or do we use `irstEmergencyAreaExclTriageAreaDescription `??
#' 
#' In acute: Identify SSP patients using `NursingUnit` = `RHS Short Stay Pediatrics`
#' 

vw_ed_mart %>% 
  filter(FacilityShortName == "RHS",
         StartDate >= "2017-01-01") %>% 
  select(FirstEmergencyAreaCode,
         FirstEmergencyAreaDescription) %>% 
  collect() %>% 
  count(FirstEmergencyAreaCode,
        FirstEmergencyAreaDescription) 


vw_ed_mart %>% 
  filter(FacilityShortName == "RHS",
         StartDate >= "2017-01-01") %>% 
  select(FirstEmergencyAreaExclTriageAreaDescription) %>% 
  collect() %>% 
  count(FirstEmergencyAreaExclTriageAreaDescription) 


vw_ed_mart %>% 
  filter(FacilityShortName == "RHS",
         StartDate >= "2017-01-01") %>% 
  select(LastEmergencyAreaDescription) %>% 
  collect() %>% 
  count(LastEmergencyAreaDescription) 


# census 
vw_census %>% 
  filter(FacilityLongName == "Richmond Hospital", 
         CensusDate >= "2017-01-01") %>% 
  select(NursingUnit) %>% 
  collect( ) %>% 
  count(NursingUnit) %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))
                           

                           



#' # SSP patients - ED LOS
#' 


#' # SSP patients - census days in 2018 
#' 
#' 

df1.census <- 
  vw_census %>% 
  filter(FacilityLongName == "Richmond Hospital", 
         CensusDate >= "2017-01-01", 
         NursingUnit == "RHS Short Stay Pediatrics") %>% 
  select(PatientID,
         AccountNum,
         NursingUnit, 
         CensusDate) %>% 
  collect( ) %>% 
  arrange(CensusDate, 
          PatientID) %>% 
  
  mutate_if(is.character, as.factor) %>% 
  mutate(PatientID = as.factor(PatientID)) %>% 
  
  mutate(census_count = 1) %>% 
  
  fill_dates(CensusDate, 
             "2017-01-01",
             "2019-11-24") %>% 
  
  replace_na(list("census_count" = 0))


str(df1.census)
summary(df1.census)

df1.census %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))


#' ## Group by day 
#' 

df2.census_by_day <- 
  df1.census %>% 
  group_by(dates_fill) %>% 
  summarise(census = sum(census_count))

df2.census_by_day %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))

df2.census_by_day %>% 
  ggplot(aes(x = dates_fill, 
             y = census)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth()


df2.census_by_day %>% 
  ggplot(aes(x = year(dates_fill) %>% as.factor(), 
             y = census)) +
  geom_boxplot() 

  
  