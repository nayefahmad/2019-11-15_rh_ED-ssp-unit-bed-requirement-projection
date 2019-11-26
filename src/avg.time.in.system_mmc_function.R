
#*********************************************************
# FUNCTION: calculate W, the steady-state avg time in system 
# for M/M/c queue 
#*********************************************************
# 2018-08-14
# Nayef Ahmad 

library("here")


# load p_0.function: 
source(here("src", 
            "p_0_pdf_function.R"))

# function defn: ------------
avg.tis_mmc <- function(lambda, 
                        mu, 
                        c,  # num servers 
                        p_0.function = p_0.prob  # function used to calculate p_0 
                        ){
      
      # inputs: 
      # > lambda: avg. arrivals per day 
      # > mu: patient turnover per stretcher per day 
      # > c: num care spaces/ED stretchers 
      # > p_0.function: prob of p_0, the steady-state prob of M/M/c
      #      queue system being in state zero (i.e. no customers in system)
      
      # output: average time in system (TIS) including time in queue and 
      #     time in service 
      # Note: for ED modelling, if we define service time as start to 
      #     disposition time, then we expect most of the TIS to fall under time in service 
      #     The division between "queue" and "service" can be arbitrarily selected for this system 
      
      # define vars: 
      r <-  lambda/mu 
      rho <- lambda/(c*mu)
      
      W = 1/mu + 
            (((r^c)/(factorial(c) * (c*mu) * ((1-rho)^2))) * 
            p_0.function(lambda, mu, c))
      
      return(W)
      
}


#********************************************
# test the fn: ---------------
#********************************************

# > Example from Gross, p72: ----------
p_0.prob(6, 3, 3) # 1/9; this is correct s

avg.tis_mmc(6, 3, 3)  # 0.4814815 hours = 28.88889 minutes; this is correct




# > Examples from VCH data: ----------

# todo: obviously, all of the below should be done in a purrr:pmap( ) loop 

# Example: VGH, calendar 2017 ------------ 
avg.tis_mmc(267, 4.08, 143) * 24  # 5.882353 hours 
# data from cube: 5.89 hours. 
# This is effectively equal! <1 minute difference!

# note: c*mu = 4.08*143 = 583.44; if lambda reaches this value, system breaks down!!

# effect of increasing visits:
df <- data.frame(visits = 267:583, # 267/day is current state; 583/day is what we expect from the forecast
           tis = map_dbl(267:583, 
                         avg.tis_mmc, 
                         c = 143, 
                         mu = 4.08, 
                         p_0.function = p_0.prob) *24) %>% 
      mutate(rho = visits/(143*4.08))
# looks like system performance starts to deteriorate sharply when rho 
#     reaches about 0.90 

scale.factor <- 1  # used in approximation for non-Markovian multi-server queue (p371 of Patient Flow book)
p1.vgh <- 
      df %>% ggplot(aes(x=rho, y=tis*scale.factor)) + 
      geom_line() + 
      scale_y_continuous(limits = c(0, 24)) +
      scale_x_continuous(breaks = seq(0.4, 1.0, 0.1)) + 
      geom_vline(xintercept = 0.9, 
                 colour = "red") + 
      labs(title = "VGH - Average time in ED versus ED stretcher utilization rate", 
           subtitle = "Queuing model shows that avg time in ED starts to increase rapidly after rho reaches 0.90", 
           x = "ED stretcher utilization (rho)", 
           y = "Time in ED (hours)") + 
      theme_classic(base_size = 12); p1.vgh

