
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
source(here::here("src", 
                  "p_0_pdf_function.R"))
source(here::here("src",
                  "avg.time.in.system_mmc_function.R")) 

#+ rest 

# 1) ----------------------------------------------------------
#' # Overview 
#' 
#' 

# 2) ----------------------------------------------------------
#' # Parameters  
#' 
start_param <- "2018-01-01"
end_param <- "2018-12-31"

# 3) ----------------------------------------------------------
#' # Identifying SSP patients 
#' In ED: Identify SSP patients using `LastEmergencyAreaDescription` = `Shortstay Peds - ED` 
#' 
#' Or do we use `irstEmergencyAreaExclTriageAreaDescription `??
#' 
#' In acute: Identify SSP patients using `NursingUnit` = `RHS Short Stay Pediatrics`
#' 

vw_ed_mart %>% 
  filter(FacilityShortName == "RHS",
         StartDate >= start_param) %>% 
  select(FirstEmergencyAreaCode,
         FirstEmergencyAreaDescription) %>% 
  collect() %>% 
  count(FirstEmergencyAreaCode,
        FirstEmergencyAreaDescription) 


vw_ed_mart %>% 
  filter(FacilityShortName == "RHS",
         StartDate >= start_param) %>% 
  select(FirstEmergencyAreaExclTriageAreaDescription) %>% 
  collect() %>% 
  count(FirstEmergencyAreaExclTriageAreaDescription) 


vw_ed_mart %>% 
  filter(FacilityShortName == "RHS",
         StartDate >= start_param) %>% 
  select(LastEmergencyAreaDescription) %>% 
  collect() %>% 
  count(LastEmergencyAreaDescription) 


# census 
vw_census %>% 
  filter(FacilityLongName == "Richmond Hospital", 
         CensusDate >= start_param) %>% 
  select(NursingUnit) %>% 
  collect( ) %>% 
  count(NursingUnit) %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))
                           

                           



#' # SSP patients - ED LOS
#' 

# 4) ----------------------------------------------------------
#' # SSP patients - census days in 2018 
#' 
#' 

df1.census <- 
  vw_census %>% 
  filter(FacilityLongName == "Richmond Hospital", 
         CensusDate >= start_param, 
         CensusDate <= end_param,
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
             start_param,
             end_param) %>% 
  
  replace_na(list("census_count" = 0))


# str(df1.census)
# summary(df1.census)

df1.census %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))

# > 4.1) ----------------------------------------------------------
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

# summary(df2.census_by_day)

df2.census_by_day %>% 
  ggplot(aes(x = dates_fill, 
             y = census)) + 
  geom_point(alpha = 0.3) + 
  geom_smooth()


df2.census_by_day %>% 
  ggplot(aes(x = year(dates_fill) %>% as.factor(), 
             y = census)) +
  geom_boxplot() 

df2.census_by_day$census %>% quantile(c(.05, .2, .5, .8, .95))

#' **There were `r df2.census_by_day$census %>% sum` census days in 2018**
#' 

# > 4.2) ----------------------------------------------------------
#' ## Group by patient 
#' How many nights does each patient stay??
#' 

df3.census_los <- 
  df1.census %>% 
  count(PatientID, 
        AccountNum) %>% 
  arrange(desc(n))
  
df3.census_los %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))
                           
df3.census_los %>% 
  filter(AccountNum != "") %>% 
  ggplot(aes(x = n)) +
  geom_density() + 
  labs(title = "RHS SSP - LOS in Acute",
       subtitle = "Looks plausibly exponential-ish", 
       x = "nights in census per stay")

df3.census_los$n %>% quantile(c(.05, .2, .5, .8, .95))

# 5) ----------------------------------------------------------
#' # Queue analysis  
#' ## Parameters 
#' 

avg_inventory <- df2.census_by_day$census %>% mean  # unit = patients 
avg_los <- df3.census_los$n %>% mean  # unit = days 

# Little's law: 
lambda = avg_inventory/avg_los

# c = 1  # num beds
mu = 1/avg_los  # avg turnover per bed per day 
rho = lambda/c*mu  # traffic intensity 


#' Find avg TIS in current state as we vary c, num beds: 
#' 
tis_1 <- sapply(2:10,  # varying c 
          avg.tis_mmc,  # function to lapply 
          lambda = lambda,  # other args 
          mu = mu)

#' What if we scale lambda by some factor? This is what 
#' will happen as the population increases. 
#' 

# define a fn: 
tis_after_scaling <- function(avg_tis_fn = avg.tis_mmc,
                              scale_param){
  sapply(2:10,  # varying c 
         avg.tis_mmc,  # function to lapply 
         lambda = lambda * scale_param,  # other args 
         mu = mu)
}

#' Vary the scale params; 
#' 
tis_2 <- tis_after_scaling(scale_param = 1.1)
tis_3 <- tis_after_scaling(scale_param = 1.3)
tis_4 <- tis_after_scaling(scale_param = 1.4)
tis_5 <- tis_after_scaling(scale_param = 1.5)
tis_6 <- tis_after_scaling(scale_param = 2.0)
tis_7 <- tis_after_scaling(scale_param = 3.0)




#' ## Avg total TIS scenarios 
#' Try several scenarios as arrival rate increases.
#' 
#' All figures in days 
#' 

df4.tis_scenarios <- 
  data.frame(num_beds = 2:10, 
             traffic_intensity_current = lambda/(2:10*mu), 
             tis_current = tis_1,
             increase_by_10_percent = tis_2, 
             increase_by_30_percent = tis_3, 
             increase_by_40_percent = tis_4, 
             increase_by_50_percent = tis_5, # %>% 
             increase_by_100_percent = tis_6) # %>% 

df4.tis_scenarios %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv"))) %>% 
  formatRound(2:8, 3)
                           
             
#' ## Avg wait in queue 
#' Try several scenarios as arrival rate increases.
#' 
#' All figures in hours. 
#' 

df5.time_in_queue_scenarios <- 
  data.frame(num_beds = 2:10, 
             traffic_intensity_current = lambda/(2:10*mu),
             wait_in_queue_current = (tis_1 - avg_los)*24,
             
             traffic_intensity_10_perc_increase = lambda*1.1/(2:10*mu),
             increase_by_10_percent = (tis_2 - avg_los)*24, 
             
             traffic_intensity_30_perc_increase = lambda*1.3/(2:10*mu),
             increase_by_30_percent = (tis_3 - avg_los)*24, 
             
             traffic_intensity_40_perc_increase = lambda*1.4/(2:10*mu),
             increase_by_40_percent = (tis_4 - avg_los)*24, 
             
             traffic_intensity_50_perc_increase = lambda*1.5/(2:10*mu),
             increase_by_50_percent = (tis_5 - avg_los)*24, 
             
             traffic_intensity_100_perc_increase = lambda*2.0/(2:10*mu),
             increase_by_100_percent = (tis_6 - avg_los)*24) # %>% 

df5.time_in_queue_scenarios %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv"))) %>% 
  formatRound(2:99, 3)

# 6) ----------------------------------------------------
#' ## Plot wait time vs traffic intensity 
#' Let's look at the relationship between rho and W_q for this system
#' in current state: 
#' 

df5.time_in_queue_scenarios %>% 
  ggplot(aes(x = traffic_intensity_current, 
             y = wait_in_queue_current)) + 
  geom_point(alpha = 0.8, 
             col = "blue") + 
  geom_text(aes(label = num_beds),
            alpha = 0.4,
            size = 3,
            col = "blue", 
            vjust = "bottom", 
            nudge_y = .1) +
  stat_smooth(se = FALSE, 
              fullrange = TRUE) + 
  
  # new layer: scenario where arrival rate increases by 50% 
  geom_point(aes(x = traffic_intensity_50_perc_increase,
                 y = increase_by_50_percent), 
             alpha = 0.8, 
             col = "red") + 
  stat_smooth(aes(x = traffic_intensity_50_perc_increase,
                  y = increase_by_50_percent),
              se = FALSE, 
              fullrange = TRUE, 
              col = "red") + 
  geom_text(aes(x = traffic_intensity_50_perc_increase,
                y = increase_by_50_percent,
                label = num_beds),
            alpha = 0.4,
            size = 3,
            col = "red", 
            vjust = "top", 
            nudge_y = -.05) +
  
  # aesthetics
  coord_cartesian(ylim = c(0, 3), 
                  xlim = c(0.10, .40)) +
  
  labs(x = "Traffic intensity", 
       y = "Wait time in queue (hours)", 
       title = "Determining number of acute SSP beds necessary", 
       subtitle = "Each curve results from varying num beds at a specific level of avg. arrival rate \n\nBlue: Current system \nRed: 50% increase in avg. arrival rate") + 
  
  annotate("segment", 
           x = df5.time_in_queue_scenarios$traffic_intensity_current[3], 
           xend = df5.time_in_queue_scenarios$traffic_intensity_current[3], 
           y = df5.time_in_queue_scenarios$wait_in_queue_current[3], 
           yend = 1.5,
           colour = "grey50") + 
  annotate("text", 
           x = df5.time_in_queue_scenarios$traffic_intensity_current[3],
           y = 1.6, 
           label = "Current state") +
  
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"))
  
                           
