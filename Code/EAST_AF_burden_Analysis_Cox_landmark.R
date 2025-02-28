# INFORMATION ##################################################################
#                                                                              #
#  Statistical Analyses for EAST-AFNET 4 AF burden (2025)                      #  
#                                                                              #
#  R 4.4.2                                                                     #
#                                                                              #
#                                                                              #
# LOAD PACKAGES ################################################################

library(dplyr)        #v 1.1.4 
library(survival)     #v 3.7-0
library(broom)        #v 1.0.7


# LOAD DATA ####################################################################

# Test_data_AF_burden.csv :
#
#  subjectnr              =  identifier patients
#  status_first_endpoint  =  Occurrence of primary composite endpoint
#                            0 - censored, 1 - event
#  time_to_first_endpoint =  Time to first composite outcome [days]
#  P_AF_1y                =  AF burden as Percentage of ECGs in AF/all ECGs
#                            recording during the first year of follow up (<365 days)
#  cid                    =  Study Center ID
#
# Data set contains only patients with available AF burden variable (P_AF_1y)

### please add path
data <- read.csv("../Data/Test_data_AF_burden.csv"
                 ,stringsAsFactors = F, sep=",")



###  ###########################################################################
# reporting the number excluded due to the event of interest or censoring prior to the landmark time
data %>% 
  filter(time_to_first_endpoint < 365) %>% 
  group_by(status_first_endpoint) %>% 
  summarize(n=n())

## 138 patients have to be excluded from analysis:
#### 117 were censored 
#### 21 had an event

# Cox regression ###############################################################

# Cox proportional hazards regression model to examine associations between
# covariates of interest and survival outcomes
## using subset argument to account for 365 day landmark (first yr follow up) 
## to exclude those patients who were censored or had event prior to the landmark time
## including Center ID (cid) as shared frailty term 

fit <- coxph(Surv(time_to_first_endpoint, status_first_endpoint) ~ P_AF_1y + frailty(cid)
              , data = data, subset = time_to_first_endpoint >= 365 )

## regression summary
summary(fit)
tidy(fit, exponentiate = T, conf.int = 0.95)


















