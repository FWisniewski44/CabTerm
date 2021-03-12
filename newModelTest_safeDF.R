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
                                                  "coal_cab", "max_poss_dur", "effec_parties_parl"))

corStructure <- round(cor(sbStructure), 2)

corrplot(corStructure, type = "upper", method = "number", order = "hclust")
d3heatmap::d3heatmap(corStructure, Rowv = FALSE, Colv=FALSE)
# exclude coal_cab (correlates: .84 with num_cabparties)

# surv object
Surv(time = sbStructure$abs_dur, event = sbStructure$discr2019, type = "right")

# model fit for part I: STRUCTURE (with coal_cab kicked due to high corr values)
coxPH.STR <- coxph(data = sbStructure, Surv(time = sbStructure$abs_dur, event = sbStructure$discr2019, type = "right") ~ . - abs_dur - discr2019 - coal_cab)
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


sbPreferences <- subset(BergmannERDDA.f, select = c("discr2019", "abs_dur", "pref_range",
                                                  "median_party_cab", "cons_cab",
                                                  "soc_cab", "parl_pref_range", "polar_barg_power"))

corPreferences <- round(cor(sbPreferences), 2)
# corPreferences <- na.omit(corPreferences)

corrplot(corPreferences, type = "upper", method = "number")
d3heatmap::d3heatmap(corPreferences, Rowv = FALSE, Colv=FALSE)
# max correlation is -.45

# surv object
Surv(time = sbPreferences$abs_dur, event = sbPreferences$discr2019, type = "right")

# model fit for part II: PREFERENCES 
coxPH.PREF <- coxph(data = sbPreferences, Surv(time = sbPreferences$abs_dur, event = sbPreferences$discr2019, type = "right") ~ . - abs_dur - discr2019)
summary(coxPH.PREF)
cox.zph(coxPH.PREF)
car::vif(coxPH.PREF)


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

corInstit <- round(cor(sbInstit), 2)

corrplot(corInstit, type = "upper", method = "number")
d3heatmap::d3heatmap(corInstit, Rowv = FALSE, Colv=FALSE)


# surv object
Surv(time = sbInstit$abs_dur, event = sbInstit$discr2019, type = "right")

# model fit for part III: INSTITUTIONS 
coxPH.INST <- coxph(data = sbInstit, Surv(time = sbInstit$abs_dur, event = sbInstit$discr2019, type = "right") ~ . 
                    - abs_dur - discr2019 - pm_pow - pm_diss_pow)
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
corBarg <- round(cor(sbBarg), 2)

corrplot(corBarg, type = "upper", method = "number")
d3heatmap::d3heatmap(corBarg, Rowv = FALSE, Colv=FALSE)


# surv object
Surv(time = sbBarg$abs_dur, event = sbBarg$discr2019, type = "right")

# model fit for part IV: BARGAINING 
coxPH.BARG <- coxph(data = sbBarg, Surv(time = sbBarg$abs_dur, event = sbBarg$discr2019, type = "right") ~ . - abs_dur - discr2019)
summary(coxPH.BARG)
cox.zph(coxPH.BARG)
car::vif(coxPH.BARG)

################ CRIT. EV. ################
# i have to see if this will be included...

# inflation_percChange
# unemploy_percChange
# v700e = total electoral volatility (unit = % of votes)

BergmannERDDA.f <- BergmannERDDA.f %>% rename("elect_volat" = "v700e")

# # subset for percentage change vars
# sbCrit.Perc <- subset(BergmannERDDA.f, select = c("discr2019", "abs_dur", "elect_volat", 
#                                              "inflation_percChange", "unemploy_percChange",
#                                              "soc_cab", "cons_cab"))

# subset for absolute vars
sbCrit.Abs <- subset(BergmannERDDA.f, select = c("discr2019", "abs_dur", "elect_volat", 
                                                 "inflation_Abs", "unemploy_Abs",
                                                 "soc_cab", "cons_cab"))

# # subset for inflation and unemployment as given in df
# sbCrit.normal <- subset(BergmannERDDA.f, select = c("discr2019", "abs_dur", "elect_volat", 
#                                                     "inflation_begin", "inflation_end",
#                                                     "unemploy_begin", "unemploy_end",
#                                                     "soc_cab", "cons_cab"))


# correlations matrix: absolute and percentage change vars
corCrit.Abs <- round(cor(sbCrit.Abs), 2)
# corCrit.Perc <- round(cor(sbCrit.Perc), 4)
# corCrit.normal <- round(cor(sbCrit.normal), 4)

# correlation heatmaps: absolute vars
corrplot(corCrit.Abs, type = "upper", method = "number")
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
coxPH.CRIT.ABS <- coxph(data = sbCrit, Surv(time = sbCrit$abs_dur, event = sbCrit$discr2019, type = "right") ~ 
                          elect_volat + unemploy_Abs + inflation_Abs -
                          soc_cab - cons_cab)
summary(coxPH.CRIT.ABS)
cox.zph(coxPH.CRIT.ABS)
# with interaction effects, the ph assumption is not met
# without this, it is very closely met, but nothing is significant

car::vif(coxPH.CRIT.ABS)
# with interactions, VIFs are unnaturally high
# without interactions, VIFs are even higher...

# # model fit: again for normal
# cox1 <- coxph(data = sbCrit.normal, Surv(time = sbCrit.normal$abs_dur, event = sbCrit.normal$discr2019, type = "right") ~ unemploy_begin + unemploy_end + 
#         elect_volat + inflation_begin + inflation_end + unemploy_begin*soc_cab + unemploy_end*soc_cab + inflation_begin*cons_cab + inflation_end*cons_cab - 
#         soc_cab - cons_cab)
# 
# summary(cox1)
# cox.zph(cox1) # ph assumption verletzt

# inflation_percChange has infinite values that R can not grasp in coxph model...



################ BEST FIT? ################
# first step: generate one big subset out of the others
sbAll <- cbind(sbBarg, sbInstit, sbPreferences, sbStructure, sbCrit.Abs)

#this HAS to be executed in this order!
sbAll[c(7, 8, 15, 16, 23, 24)] <- list(NULL)
sbAll[22] <- NULL
sbAll[22] <- NULL
sbAll[c(23, 24)] <- list(NULL)
sbAll[c(26, 27)] <- list(NULL)

# sbAll.step <- sbAll
# sbAll.step[c(1, 2)] <- list(NULL)

# # second step: survObj for sbAll
# Surv(time = sbAll$abs_dur, event = sbAll$discr2019)
# 
# # third step: model fit and misc for sbAll
# coxPH.ALL <- coxph(data = sbAll, Surv(time = sbAll$abs_dur, event = sbAll$discr2019) ~ . -abs_dur - discr2019)
# summary(coxPH.ALL)
# cox.zph(coxPH.ALL)

# get rid of NAs
sbAll <- na.omit(sbAll)

# null model and full model
modNull <- coxph(data = sbAll, Surv(time = sbAll$abs_dur, event = sbAll$discr2019) ~ 1)
modFull <- coxph(data = sbAll, Surv(time = sbAll$abs_dur, event = sbAll$discr2019) ~ . -abs_dur - discr2019)

# step function for acquiring best fit
stepAIC(modNull, scope = list(upper = modFull), direction = "both")

# best fit as object with summary and test for ph assumption
best.fit <- coxph(formula = Surv(time = sbAll$abs_dur, event = sbAll$discr2019) ~ 
                    cabunan + effec_parties_parl + erdda_bicam + pref_range + 
                    parl_pref_range + unemploy_Abs + num_cabparties + elect_volat + 
                    median_party_cab + erdda_semip, data = sbAll)
summary(best.fit)
cox.zph(best.fit)
car::vif(best.fit)

################ SAVE DF ##################
save(BergmannERDDA.f, file = "BergmannERDDA.f.RData")
save(sbAll, file = "sbAll.RData")



















########################################### correlation tests! (do this for each level individually)

attach(sbAll)
detach(sbAll)

# basic corr matrix for sbAll
cormat <- round(cor(sbAll), 2)
meltedCormat <- reshape2::melt(cormat)
ggplot(data = meltedCormat, aes(x = Var1, y = Var2, fill = value)) + geom_raster()

###########

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

# ###########
# 
# ## use functions
# lowerTri <- get_lower_tri(cormat)
# upperTri <- get_upper_tri(cormat)
# 
# # melting triangular parts
# meltUpper <- reshape2::melt(upperTri, na.rm = T)
# meltLower <- reshape2::melt(lowerTri, na.rm = T)

# # better heatmap [see this article](http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization)
# plotUpperTri <- ggplot(data = meltUpper, aes(Var2, Var1, fill = value)) + 
#   geom_raster(color = "white")+
#   scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
#                        midpoint = 0, limit = c(-1,1), space = "Lab", 
#                        name="Pearson\nCorrelation") +
#   theme_minimal()+ 
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, 
#                                    size = 12, hjust = 1))+
#   coord_fixed()
# 
# plotUpperTri + geom_text(aes(Var2, Var1, label = value), color = "black", size = 2)
# 
# print(plotUpperTri)

# this may be the best heatmap for .Rmd
corrplotAll <- corrplot::corrplot(cormat, type = "upper", method = "circle", order = "hclust")
print(corrplotAll)

# this is the best heatmap: interactive
d3corrplotAll <- d3heatmap::d3heatmap(cormat, Rowv = F, Colv = F)
print(d3corrplotAll)

# results:
## .87 corr: coal_cab X num_cabparties
## .78 corr: pref_range X num_cabparties



# ## test corrs between num_cabparties X pref_range, also coal_cab X pref_range
# ## also test invest X erdda_bicam
# 
# # kick out candidates
# 
# corr.test(sbAll$num_cabparties, sbAll$pref_range, method = "pearson")
# # .78 --> maybe kick one var out
# 
# corr.test(sbAll$num_cabparties, sbAll$coal_cab, method = "pearson")
# # .87 --> high correlation, kick one out
# 
# # further testing
# 
# corr.test(sbAll$coal_cab, sbAll$pref_range, method = "pearson")
# # .68
# 
# corr.test(sbAll$invest, sbAll$erdda_bicam, method = "pearson")
# 
# corr.test(sbAll$num_cabparties, sbAll$coal_cab, method = "pearson")
# 
# # testing negative corrs
# corr.test(sbAll$invest, sbAll$parl_pref_range, method = "pearson")
# # -.51 - as this is the "darkest" blue spot in the heatmap, rest can safely be ignored
