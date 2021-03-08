# libraries survival analysis
library(survival)
library(survminer)
library(survMisc)
library(simPH)
library(flexsurv)

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

########################################### 

# loading: in case it is needed
## BerJoi <- load(file = "/home/fw/Documents/GitHub Projects/CabTerm/BerJoi.RData")

# a first test of cox models with the relevant df and vars
attach(BerJoi)

# help("Surv")
#
## for safety reasons, df is specified here
SurvObj <- Surv(BerJoi$abs_dur, event = BerJoi$discr2019)

# base syntax coxph
# help("coxph")

## kaplan meier model - DISCR19
kmModFULL <- survfit(SurvObj ~ 1)
summary(kmModFULL)

## kaplan meier plot - discr19
plot(kmModFULL, conf.int = F, xlab = "Time of Survival (in days)", ylab = "% of Cabinets 'alive'",
     main = "Kaplan-Meier-Plot, pooled hazards")


# # test for full model, whole df - syntax reference
# baseCPH <- coxph(data = BerJoi, SurvObj ~ 1)
# fullCPH <- coxph(data = BerJoi, SurvObj ~ .)

########################################### 

# subsetting
subset1 <- subset(BerJoi, select = c("abs_dur", "discr2019", "minor_coalition", 
                                     "one_party_minor", "post_election_cabinet","num_cabparties", "max_dur", 
                                     "eff_numb_parties", "gov_comp", "right_seat", "comm_seat", 
                                     "rl_range", "polarization_bpw", "antisys_seat", 
                                     "positive_parl", "pm_diss_pow", "bicameralism", 
                                     "semi_presidentialism", "same_pm", "cab_barg_duration"))
#######

# subset with independent vars only for correlations
subset2 <- subset(BerJoi, select = c("num_cabparties", "max_dur", 
                                     "eff_numb_parties", "gov_comp", "right_seat", "comm_seat", 
                                     "rl_range", "polarization_bpw", "antisys_seat", 
                                     "positive_parl", "pm_diss_pow", "bicameralism", 
                                     "semi_presidentialism", "same_pm", "cab_barg_duration"))


detach(BerJoi)
#
fre(subset1$one_party_minor)
fre(subset1$minor_coalition)
fre(subset1$post_election_cabinet)

# sub surv object
Surv(subset1$abs_dur, event = subset1$discr2019)

# kaplan plots
subKap <- survfit(Surv(subset1$abs_dur, event = subset1$discr2019) ~ 1)

plot(subKap, conf.int = F, xlab = "Time of Survival (in days)", ylab = "% of Cabinets 'alive'",
     main = "Kaplan-Meier-Plot for subset1, pooled hazards")

# testmodel cox subset1 (for syntax reference)
testSub <- coxph(data = subset1, Surv(subset1$abs_dur, event = subset1$discr2019) ~ . - comm_seat - pm_diss_pow - gov_comp - bicameralism - post_election_cabinet - minor_coalition - one_party_minor)
summary(testSub)

cox.zph(testSub)
ggcoxzph(cox.zph(testSub))

# test for correlations between vars
attach(subset1)

PerformanceAnalytics::chart.Correlation(subset1)

# basic corr matrix
cormat <- cor(subset1 - one_party_minor - minor_coalition - post_election_cabinet)
meltedCormat <- reshape2::melt(cormat)
ggplot(data = meltedCormat, aes(x = Var1, y = Var2, fill = value)) + geom_raster()

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

## use functions
lowerTri <- get_lower_tri(cormat)
upperTri <- get_upper_tri(cormat)

# melting upperTri
meltUpper <- reshape2::melt(upperTri, na.rm = T)
meltLower <- reshape2::melt(lowerTri, na.rm = T)

# better heatmap [see this article](http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization)
plotUpperTri <- ggplot(data = meltUpper, aes(Var2, Var1, fill = value)) + 
  geom_raster(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

#############

plotLowerTri <- ggplot(data = meltLower, aes(Var2, Var1, fill = value))+
  geom_raster(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

###############################################

# basic corr matrix - just independent vars
cormat <- cor(subset2)
meltedCormat <- reshape2::melt(cormat)
ggplot(data = meltedCormat, aes(x = Var1, y = Var2, fill = value)) + geom_raster()

## use functions for triangular matrix parts on indep vars
lowerTri <- get_lower_tri(cormat)
upperTri <- get_upper_tri(cormat)

# melting upperTri
meltUpper <- reshape2::melt(upperTri, na.rm = T)
meltLower <- reshape2::melt(lowerTri, na.rm = T)

# better heatmap [see this article](http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization)
plotUpperTri <- ggplot(data = meltUpper, aes(Var2, Var1, fill = value)) + 
  geom_raster(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

#############

plotLowerTri <- ggplot(data = meltLower, aes(Var2, Var1, fill = value))+
  geom_raster(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

############################################### model STRUCTURE
attach(BerJoi)
detach(BerJoi)
############################################### 

# subset STRUCTURE

subSTRUCT <- subset(BerJoi, select = c("discr2019", "abs_dur", "num_cabparties", "max_dur", "eff_numb_parties"))

##########

survSTRUCT <- Surv(subSTRUCT$abs_dur, event = subSTRUCT$discr2019)

summary(coxph(data = subSTRUCT, survSTRUCT ~ . - discr2019 - abs_dur))
cox.zph(coxph(data = subSTRUCT, survSTRUCT ~ . - discr2019 - abs_dur))
# sig: effective number of parliament. parties significant - the more parties, the higher the risk of disc2019

############################################### model PREFERENCES

subPREF <- subset(BerJoi, select = c("discr2019", "abs_dur", "rl_polar", "right_seat",
                                     "comm_seat", "rl_range", "polarization_bpw", "antisys_seat"))

##########

survPREF <- Surv(subPREF$abs_dur, event = subPREF$discr2019)

summary(coxph(data = subPREF, survPREF ~ . - discr2019 - abs_dur))
cox.zph(coxph(data = subPREF, survPREF ~ . - discr2019 - abs_dur))
# sig: left party seat share: the higher the amount of left seats, the higher the risk

############################################### model INSTITUTIONS

subINST <- subset(BerJoi, select = c("discr2019", "abs_dur", "positive_parl", "pm_diss_pow",
                                     "bicameralism", "semi_presidentialism")) 

##########

survINST <- Surv(subINST$abs_dur, event = subINST$discr2019)

summary(coxph(data = subINST, survINST ~ . - discr2019 - abs_dur))
cox.zph(coxph(data = subINST, survINST ~ . - discr2019 - abs_dur))
# sig: PM dissolution power: the higher the PM diss pow, the lower the risk

############################################### model BARGAINING

subBARG <- subset(BerJoi, select = c("discr2019", "abs_dur", "same_pm", "cab_barg_duration", "max_dur"))

##########

survBARG <- Surv(subBARG$abs_dur, event = subBARG$discr2019)

summary(coxph(data = subBARG, survBARG ~ . - discr2019 - abs_dur))
cox.zph(coxph(data = subBARG, survBARG ~ . - discr2019 - abs_dur))
# sig: borderline significant: same PM - when same PM in cabinet, the lower the risk
# sig: cabinet bargaining duration: the longer it takes, the higher the risk of failure

############################################### model CRITICAL EVENTS

# subCE <- 

