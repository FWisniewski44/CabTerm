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
library(rms)
library(MASS)
library(corrplot)
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
library(d3heatmap)
library(kableExtra)
library(gtools)

########################################### models test with safely merged df

BergmannERDDA <- read_dta(file = "/Users/flo/Documents/GitHub/CabTerm/erdda2014-Bergmann2019_merged.dta")
BergmannERDDA <- as_tibble(BergmannERDDA)

fre(BergmannERDDA$discr2019)
# test
fre(combo$discr2019)
fre(BergmannERDDA$discr2019)

################################## trial and error: mutating vars for inflation etc

# we want to acquire variables for the critical events perspective in the paper.
# therefore, some calculations have to be made; first, rename all relevant variables.
# in a second step, use mutate to get absolute difference values between performance vars
# for each cabinet. then, in a third step, use this absolute difference to calculate the 
# percentage change - these two can be used as values to check the overall performance of
# cabinets in economic terms such as inflation, unemployment and economic growth - also in
# comparison to former governments.

# rename relevant vars
BergmannERDDA <- BergmannERDDA %>% rename("inflation_begin" = "v702e", 
                                          "inflation_end" = "v706e")

BergmannERDDA <- BergmannERDDA %>% rename("unemploy_begin" = "v703e",
                                          "unemploy_end" = "v705e")

BergmannERDDA <- BergmannERDDA %>% rename("growth_begin" = "v704e",
                                          "growth_end" = "v707e")

# test: set 99999 to 1 and Inf to 1
BergmannERDDA[BergmannERDDA == 99999] <- NA
BergmannERDDA[BergmannERDDA == Inf] <- NA
BergmannERDDA[BergmannERDDA == -Inf] <- NA
BergmannERDDA[BergmannERDDA == 88888] <- NA


# mutate: we want precentage difference. calculate abs. diff. first

# INFLATION: ABSOLUTE DIFF.
BergmannERDDA <- BergmannERDDA %>%
  mutate(inflation_Abs = (inflation_end - inflation_begin))

fre(BergmannERDDA$inflation_Abs)

# # INFLATION: % DIFF.
# BergmannERDDA <- BergmannERDDA %>%
#   mutate(inflation_percChange = (inflation_Abs / inflation_begin)*100)
# 
# fre(BergmannERDDA$inflation_percChange)

#####

# UNEMPLOYMENT: ABSOLUTE DIFF.
BergmannERDDA <- BergmannERDDA %>%
  mutate(unemploy_Abs = (unemploy_end - unemploy_begin))

fre(BergmannERDDA$unemploy_Abs)

# # UNEMPLOYMENT: % DIFF.
# BergmannERDDA <- BergmannERDDA %>%
#   mutate(unemploy_percChange = (unemploy_Abs / unemploy_begin)*100)
# 
# fre(BergmannERDDA$unemploy_percChange)

#####

# GROWTH: ABSOLUTE DIFF.
BergmannERDDA <- BergmannERDDA %>%
  mutate(growth_Abs = (growth_end - growth_begin))

fre(BergmannERDDA$growth_Abs)

# # GROWTH: % DIFF.
# BergmannERDDA <- BergmannERDDA %>%
#   mutate(growth_percChange = (growth_Abs/growth_begin)*100)
# 
# fre(BergmannERDDA$growth_percChange)

# which(grepl(-58.1, BergmannERDDA$growth_Abs))
# 
# which(grepl(-5100, BergmannERDDA$growth_percChange))

########################################### 

BergmannERDDA$v329e

# this: (v329e == 1 is minority government) --> we get 207 cases of minority cabs
BergmannERDDA %>% filter(v329e == 1)
combo %>% filter(v329e == 1)

# or this (filter by minor_coalition and one_party_minor) --> we get same # of cases of minority cabs
BergmannERDDA.f <- BergmannERDDA %>% filter(minor_coalition == 1 | one_party_minor == 1)
combo %>% filter(minor_coalition == 1 | one_party_minor == 1)

# filter: non partisan govs // edit: value is 0 for all 207 obs.
fre(BergmannERDDA.f$non_partisan_cabinet)

# test: how's it going with my calculated vars?
fre(BergmannERDDA.f$inflation_Abs)
fre(BergmannERDDA.f$unemploy_Abs)
fre(BergmannERDDA.f$growth_Abs)


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
                                                  "max_poss_dur", "effec_parties_parl"))

# correlations
# sbStructure <- na.omit(sbStructure)

corStructure <- round(cor(na.omit(sbStructure)), 2)

# corrplot(corStructure, type = "upper", method = "circle", order = "hclust")
d3heatmap::d3heatmap(corStructure, Rowv = FALSE, Colv=FALSE)
# exclude coal_cab (correlates: .84 with num_cabparties)
# exclude one_party_minor(high correlation with num_cabparties, -.84)

# surv object
Surv(time = sbStructure$abs_dur, event = sbStructure$discr2019, type = "right")

# model fit for part I: STRUCTURE (with coal_cab kicked due to high corr values)
coxPH.STR <- coxph(data = sbStructure, Surv(time = sbStructure$abs_dur, event = sbStructure$discr2019, type = "right") ~ . - 
                     abs_dur - discr2019)
extractAIC(coxPH.STR)
summary(coxPH.STR)
cox.zph(coxPH.STR)
car::vif(coxPH.STR)

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

BergmannERDDA.f <- BergmannERDDA.f %>% rename("connected_cab" = "v413e")
fre(BergmannERDDA.f$connected_cab)

sbPreferences <- subset(BergmannERDDA.f, select = c("discr2019", "abs_dur", "pref_range",
                                                  "median_party_cab", "cons_cab", "connected_cab",
                                                  "soc_cab", "parl_pref_range", "antisys_seat"))

# sbPrefTest <- subset(BergmannERDDA.f, select = c("discr2019", "abs_dur", "pref_range",
#                                                  "median_party_cab", "cons_cab", "connected_cab",
#                                                  "soc_cab", "polar_barg_power"))

# correlations

# na.omit preferences
# sbPreferences <- na.omit(sbPreferences)

# corrmatrix
corPreferences <- round(cor(na.omit(sbPreferences)), 2)
# corPrefTest <- round(cor(na.omit(sbPrefTest)), 2)

# heatmap
# corrplot(corPreferences, type = "upper", method = "number", addCoefasPercent = T)
d3heatmap::d3heatmap(corPreferences, Rowv = FALSE, Colv=FALSE)
# with NAs excluded: polar_barg_power (.81 with parl_pref_range)

# d3heatmap(corPrefTest, Rowv = F, Colv = F)

# surv object
Surv(time = sbPreferences$abs_dur, event = sbPreferences$discr2019, type = "right")

# Surv(time = sbPrefTest$abs_dur, event = sbPrefTest$discr2019, type = "right")

# model fit for part II: PREFERENCES 
coxPH.PREF <- coxph(data = sbPreferences, Surv(time = sbPreferences$abs_dur, event = sbPreferences$discr2019, type = "right") ~ . - abs_dur - discr2019)
extractAIC(coxPH.PREF)
summary(coxPH.PREF)
cox.zph(coxPH.PREF)
car::vif(coxPH.PREF)

# coxPH.PREF_T <- coxph(data = sbPrefTest, Surv(time = sbPrefTest$abs_dur, event = sbPrefTest$discr2019, type = "right") ~ . - abs_dur - discr2019)
# extractAIC(coxPH.PREF_T)
# summary(coxPH.PREF_T)
# cox.zph(coxPH.PREF_T)
# car::vif(coxPH.PREF_T)


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
                                               "pm_diss_pow", "erdda_bicam", "erdda_semip",
                                               "post_election_cabinet"))

# correlations

# na.omit institutions
# sbInstit <- na.omit(sbInstit)

# corrmatrix institutions
corInstit <- round(cor(na.omit(sbInstit)), 2)

# heatmaps
# corrplot(corInstit, type = "upper", method = "number", order = "hclust")
d3heatmap::d3heatmap(corInstit, Rowv = FALSE, Colv = FALSE)
# exclude no var, no high correlations
# highest is erdda_bicam and invest, which is theoretically explainable


# surv object
Surv(time = sbInstit$abs_dur, event = sbInstit$discr2019, type = "right")

# model fit for part III: INSTITUTIONS 
coxPH.INST <- coxph(data = sbInstit, Surv(time = sbInstit$abs_dur, event = sbInstit$discr2019, type = "right") ~ . 
                    - abs_dur - discr2019 - pm_pow - pm_diss_pow)
extractAIC(coxPH.INST)
summary(coxPH.INST)
cox.zph(coxPH.INST)
car::vif(coxPH.INST)

################ BARGAINING ###############
  ## same_pm
# v600e = cabinet bargaining duration (unit = # of days)
  ## cab_barg_duration
# v329e = type of government (1 = minority, 2 = MWC, 3 = surplus)
# already recoded v305e to max_poss_dur

BergmannERDDA.f <- BergmannERDDA.f %>% rename("erdda_barg_dur" = "v600e",
                                              "govtype" = "v329e")


sbBarg <- subset(BergmannERDDA.f, select = c("discr2019", "abs_dur", "same_pm",
                                               "erdda_barg_dur", "max_poss_dur", "coal_cab"))

# correlations
# sbBarg <- na.omit(sbBarg)

corBarg <- round(cor(na.omit(sbBarg)), 2)

# corrplot(corBarg, type = "upper", method = "number", order = "hclust")
d3heatmap::d3heatmap(corBarg, Rowv = FALSE, Colv=FALSE)
# exclude no var, no high correlations

# surv object
Surv(time = sbBarg$abs_dur, event = sbBarg$discr2019, type = "right")

# model fit for part IV: BARGAINING 
coxPH.BARG <- coxph(data = sbBarg, Surv(time = sbBarg$abs_dur, event = sbBarg$discr2019, type = "right") ~ . - abs_dur - discr2019)
summary(coxPH.BARG)
cox.zph(coxPH.BARG)
car::vif(coxPH.BARG)

################ CRIT. EV. ################
# i have to see if this will be included... (update: yes, it will be included)

# inflation_percChange
# unemploy_percChange
# v700e = total electoral volatility (unit = % of votes)

BergmannERDDA.f <- BergmannERDDA.f %>% rename("elect_volat" = "v700e")

sbCrit.Abs <- subset(BergmannERDDA.f, select = c("discr2019", "abs_dur", "elect_volat", 
                                                 "inflation_Abs", "unemploy_Abs",
                                                 "soc_cab", "cons_cab"))

# correlations
# sbCrit.Abs <- na.omit(sbCrit.Abs)

corCrit.Abs <- round(cor(na.omit(sbCrit.Abs)), 2)

# heatmaps
# corrplot(corCrit.Abs, type = "upper", method = "number", order = "hclust")
d3heatmap::d3heatmap(corCrit.Abs, Rowv = FALSE, Colv=FALSE)

# # correlation heatmaps: percentage change vars
# corrplot(corCrit.Perc, type = "upper", method = "number")
# d3heatmap::d3heatmap(corCrit.Perc, Rowv = FALSE, Colv=FALSE)
# 
# # correlation heatmaps: normal sb
# corrplot(corCrit.normal, type = "upper", method = "number")
# d3heatmap::d3heatmap(corCrit.normal, Rowv = FALSE, Colv=FALSE)


# surv object
Surv(time = sbCrit$abs_dur, event = sbCrit$discr2019, type = "right")

# model fit for part V: CRITICAL EVENTS 
coxPH.CRIT.ABS <- coxph(data = sbCrit.Abs, Surv(time = sbCrit.Abs$abs_dur, event = sbCrit.Abs$discr2019, type = "right") ~ 
                          elect_volat + unemploy_Abs + inflation_Abs -
                          soc_cab - cons_cab)
extractAIC(coxPH.CRIT.ABS)
summary(coxPH.CRIT.ABS)
cox.zph(coxPH.CRIT.ABS)
car::vif(coxPH.CRIT.ABS)
# with interaction effects, the ph assumption is not met
# without this, it is mostly met (except for inflation var), but if i delete that one, it is met but
# what does this tell me? nothing...

car::vif(coxPH.CRIT.ABS)
# with interactions, VIFs are unnaturally high
# VIFs are ok if interactions are left out




################ BEST FIT ################
# first step: generate one big subset out of the others
sbAll <- cbind(sbBarg, sbInstit, sbPreferences, sbStructure, sbCrit.Abs)

#this HAS to be executed in this order!
## delete double discr2019 and abs_dur
sbAll[c(7, 8, 16, 17, 25, 26, 32, 33)] <- list(NULL)
## delete other double vars
### max_poss_dur
sbAll[24] <- NULL
### soc_cab and cons_cab
sbAll[c(28, 29)] <- list(NULL)


# sbAll.step <- sbAll
# sbAll.step[c(1, 2)] <- list(NULL)

# # second step: survObj for sbAll
# Surv(time = sbAll$abs_dur, event = sbAll$discr2019)
# 
# # third step: model fit and misc for sbAll
# coxPH.ALL <- coxph(data = sbAll, Surv(time = sbAll$abs_dur, event = sbAll$discr2019) ~ . -abs_dur - discr2019)
# summary(coxPH.ALL)
# cox.zph(coxPH.ALL)

# correlations

# corrmatrix
corAll <- round(cor(na.omit(sbAll)), 2)

d3heatmap(corAll, Rowv = F, Colv = F)

# get rid of high correlates:
sbAll$coal_cab <- NULL
sbAll$pref_range <- NULL

# sbAll$num_cabparties <- NULL


# get rid of NAs
sbAll <- na.omit(sbAll)

# null model and full model
modNull <- coxph(data = sbAll, Surv(time = sbAll$abs_dur, event = sbAll$discr2019, type = "right") ~ 1)
modFull <- coxph(data = sbAll, Surv(time = sbAll$abs_dur, event = sbAll$discr2019, type = "right") ~ . -abs_dur - discr2019)

# otherCPH1 <- cph(data = sbAll, Surv(time = sbAll$abs_dur, event = sbAll$discr2019) ~ 1, surv = T, method = "breslow")
# summary(otherCPH1)
# otherCPH2 <- cph(data = sbAll, Surv(time = sbAll$abs_dur, event = sbAll$discr2019) ~ . -abs_dur - discr2019, surv = T, method = "breslow")

# step function for acquiring best fit, direction = both
stepAIC(modNull, scope = list(upper = modFull), direction = "both")

# step function with direction = backward
# stepAIC(modFull, scope = list(upper = modNull), direction = "backward")
# 
# backward <- coxph(formula = Surv(time = sbAll$abs_dur, event = sbAll$discr2019, type = "right") ~ 
#                     same_pm + erdda_barg_dur + cabunan + erdda_bicam + 
#                     post_election_cabinet + parl_pref_range + antisys_seat + 
#                     cab_seat_share + effec_parties_parl + unemploy_Abs, data = sbAll)
# cox.zph(backward)
# summary(backward)

# is also possible with rms package (by Frank Harrell), but as i have just used this before for polr,
# i will just list this as a second possible way of obtaining a step function fit
# fastbw(rule = "p", fit = otherCPH1)

# # backward
# coxph(formula = Surv(time = sbAll$abs_dur, event = sbAll$discr2019, 
# type = "right") ~ same_pm + erdda_barg_dur + cabunan + erdda_bicam + 
#   post_election_cabinet + pref_range + parl_pref_range + antisys_seat + 
#   cab_seat_share + num_cabparties + effec_parties_parl + elect_volat + 
#   unemploy_Abs, data = sbAll)

# both
coxph(formula = Surv(time = sbAll$abs_dur, event = sbAll$discr2019, 
                     type = "right") ~ erdda_bicam + effec_parties_parl + parl_pref_range + 
        post_election_cabinet + erdda_barg_dur + same_pm + cab_seat_share, 
      data = sbAll)




# best fit as object with summary and test for ph assumption
best.fit <- coxph(formula = Surv(time = sbAll$abs_dur, event = sbAll$discr2019, type = "right") ~ 
                    erdda_bicam + effec_parties_parl + parl_pref_range + 
                    post_election_cabinet + erdda_barg_dur + same_pm + cab_seat_share, data = sbAll)
extractAIC(best.fit)
summary(best.fit)
cox.zph(best.fit)
car::vif(best.fit)

################ SAVE DF ##################
save(BergmannERDDA.f, file = "BergmannERDDA.f.RData")
save(sbAll, file = "sbAll.RData")


fre(BergmannERDDA.f$country)
names(BergmannERDDA.f$country)


# EOD
