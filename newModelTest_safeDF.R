# libraries survival analysis
library(survival)
library(survminer)
library(survMisc)
library(simPH)
library(flexsurv)
library(My.stepwise)

# libraries general
library(tidymodels)
library(tidyverse)
library(MASS)
library(lubridate)
library(Amelia)
library(xtable)
library(haven)
library(foreign)
library(broom)
library(psych)
library(Hmisc)
library(expss)
library(rockchalk)
library(effects)
library(modeldata)
library(randomForest)
library(compare)
library(psych)
library(desc)
library(PerformanceAnalytics)
library(reshape2)

########################################### models test with safely merged df

BergmannERDDA <- read_dta(file = "/Users/flo/Documents/GitHub/CabTerm/erdda2014-Bergmann2019_merged.dta")
BergmannERDDA <- as_tibble(BergmannERDDA)

fre(BergmannERDDA$discr2019)
# test
fre(combo$discr2019)
fre(BergmannERDDA$discr2019)

########################################### 

BergmannERDDA$v329e

# this: (v329e == 1 is minority government) --> we get 207 cases of minority cabs
BergmannERDDA %>% filter(v329e == 1)
combo %>% filter(v329e == 1)

# or this (filter by minor_coalition and one_party_minor) --> we get same # of cases of minority cabs
BergmannERDDA.f <- BergmannERDDA %>% filter(minor_coalition == 1 | one_party_minor == 1)
combo %>% filter(minor_coalition == 1 | one_party_minor == 1)

###########################################

# corr matrix: upper and lower triangular parts of matrices
## function lower tri
get_lower_tri <- function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

## function upper tri
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}

########################################### recoding needed vars from erdda-part

################ STRUCTURE ################
# v318e = cabinet seat share (unit = %points)
  ## num_cabparties
# v322e = party with max. bargaining power is in cabinet (yes/no)
# v316e = coalition cabinet (yes/no)
# v305e = maximum possible cab duration (unit = # of days)
  ## max_dur
# v309e = effective number of parliamentary parties (unit = # of parties)
  ## eff_numb_parties

### rename vars

BergmannERDDA.f <- BergmannERDDA.f %>% rename("cab_seat_share" = "v318e",
                                              "max_bargpow_cab" = "v322e",
                                              "coal_cab" = "v316e",
                                              "max_poss_dur" = "v305e",
                                              "effec_parties_parl" = "v309e")


sbStructure <- subset(BergmannERDDA.f, select = c("discr2019", "abs_dur", "cab_seat_share",
                                                  "num_cabparties", "max_bargpow_cab",
                                                  "coal_cab", "max_poss_dur", "effec_parties_parl"))


# surv object
Surv(time = sbStructure$abs_dur, event = sbStructure$discr2019)

# model fit for part I: STRUCTURE 
coxPH.STR <- coxph(data = sbStructure, Surv(time = sbStructure$abs_dur, event = sbStructure$discr2019) ~ . - abs_dur - discr2019)
summary(coxPH.STR)
cox.zph(coxPH.STR)

################ PREFERENCES ##############
# v410e = cabinet preference range (unit = manifesto points)
  ## rl_polar?
# v411e = median party (1st dimension) in cabinet (yes/no)
# v415e = conservative cab (yes/no)
# v416e = socialist cab (yes/no)
# v406e = parliamentary preference range (unit???)
  ## rl_range
# v407e = polarization bargaining power weighted (unit = manifesto points)
  ## polarization_bpw
  ## antisys_seat

### rename vars

BergmannERDDA.f <- BergmannERDDA.f %>% rename("pref_range" = "v410e",
                                              "median_party_cab" = "v411e",
                                              "cons_cab" = "v415e",
                                              "soc_cab" = "v416e",
                                              "parl_pref_range" = "v406e",
                                              "polar_barg_power" = "v407e")


sbPreferences <- subset(BergmannERDDA.f, select = c("discr2019", "abs_dur", "pref_range",
                                                  "median_party_cab", "cons_cab",
                                                  "soc_cab", "parl_pref_range", "polar_barg_power"))


# surv object
Surv(time = sbPreferences$abs_dur, event = sbPreferences$discr2019)

# model fit for part I: STRUCTURE 
coxPH.PREF <- coxph(data = sbPreferences, Surv(time = sbPreferences$abs_dur, event = sbPreferences$discr2019) ~ . - abs_dur - discr2019)
summary(coxPH.PREF)
cox.zph(coxPH.PREF)



################ INSTITUTIONS #############
# v505e = positive parliamentarism (yes/no)
# v509e = cabinet decisions: unanimity rule (yes/no)
# v513e = PM powers (scale: 1-7)
  ## pm_diss_pow = PM dissolution powers
# v504e = bicameralism (yes/no)
  ## bicameralism
# v518e = semi-presidentialism (yes/no)
  ## semi_presidentialism

### rename vars

BergmannERDDA.f <- BergmannERDDA.f %>% rename("invest" = "v505e",
                                              "cabunan" = "v509e",
                                              "pm_pow" = "v513e",
                                              "erdda_bicam" = "v504e",
                                              "erdda_semip" = "v518e")


sbInstit <- subset(BergmannERDDA.f, select = c("discr2019", "abs_dur", "invest",
                                               "cabunan", "pm_pow",
                                               "pm_diss_pow", "erdda_bicam", "erdda_semip"))


# surv object
Surv(time = sbInstit$abs_dur, event = sbInstit$discr2019)

# model fit for part I: STRUCTURE 
coxPH.INST <- coxph(data = sbInstit, Surv(time = sbInstit$abs_dur, event = sbInstit$discr2019) ~ . - abs_dur - discr2019)
summary(coxPH.INST)
cox.zph(coxPH.INST)

################ BARGAINING ###############
  ## same_pm
# v600e = cabinet bargaining duration (unit = # of days)
  ## cab_barg_duration
# v329e = type of government (1 = minority, 2 = MWC, 3 = surplus)
# already recoded v305e to max_poss_dur

BergmannERDDA.f <- BergmannERDDA.f %>% rename("erdda_barg_dur" = "v600e",
                                              "govtype" = "v329e")


sbBarg <- subset(BergmannERDDA.f, select = c("discr2019", "abs_dur", "same_pm",
                                               "erdda_barg_dur", "max_poss_dur"))


# surv object
Surv(time = sbBarg$abs_dur, event = sbBarg$discr2019)

# model fit for part I: STRUCTURE 
coxPH.BARG <- coxph(data = sbBarg, Surv(time = sbBarg$abs_dur, event = sbBarg$discr2019) ~ . - abs_dur - discr2019)
summary(coxPH.BARG)
cox.zph(coxPH.BARG)

################ CRIT. EV. ################
# i have to see if this will be included...


################ BEST FIT? ################
# first step: generate one big subset out of the others
sbAll <- cbind(sbBarg, sbInstit, sbPreferences, sbStructure)

#this HAS to be executed in this order!
sbAll[c(6, 7, 14, 15, 22, 23)] <- list(NULL)
sbAll[22] <- NULL

# sbAll.step <- sbAll
# sbAll.step[c(1, 2)] <- list(NULL)

# second step: survObj for sbAll
Surv(time = sbAll$abs_dur, event = sbAll$discr2019)

# third step: model fit and misc for sbAll
coxPH.ALL <- coxph(data = sbAll, Surv(time = sbAll$abs_dur, event = sbAll$discr2019) ~ . -abs_dur - discr2019)
summary(coxPH.ALL)
cox.zph(coxPH.ALL)

# get rid of NAs
sbAll <- na.omit(sbAll)

# null model and full model
modNull <- coxph(data = sbAll, Surv(time = sbAll$abs_dur, event = sbAll$discr2019) ~ 1)
modFull <- coxph(data = sbAll, Surv(time = sbAll$abs_dur, event = sbAll$discr2019) ~ . -abs_dur - discr2019)

# step function for acquiring best fit
stepAIC(modNull, scope = list(upper = modFull), direction = "both")

# best fit as object with summary and test for ph assumption
best.fit <- coxph(data = sbAll, Surv(time = sbAll$abs_dur, event = sbAll$discr2019) ~ soc_cab + cons_cab + effec_parties_parl + cabunan + erdda_bicam + max_poss_dur + cab_seat_share + same_pm + pm_pow + erdda_semip)
summary(best.fit)
cox.zph(best.fit)


################ SAVE DF ##################
save(BergmannERDDA.f, file = "BergmannERDDA.f.RData")

########################################### works, begin building models


################ STRUCTURE ################





