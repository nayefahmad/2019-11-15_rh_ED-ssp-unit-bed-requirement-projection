
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

setup_sql_server()



#+ rest 

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
  select(LastEmergencyAreaDescription) %>% 
  collect() %>% 
  count(LastEmergencyAreaDescription) 


  
vw_ed_mart %>% 
  filter(FacilityShortName == "RHS",
         StartDate >= "2017-01-01") %>% 
  select(FirstEmergencyAreaUnitDescription) %>% 
  collect() %>% 
  count(FirstEmergencyAreaUnitDescription) 



vw_ed_mart %>% 
  filter(FacilityShortName == "RHS",
         StartDate >= "2017-01-01") %>% 
  select(InpatientNursingUnitName) %>% 
  collect() %>% 
  count(InpatientNursingUnitName) %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))
                           



  