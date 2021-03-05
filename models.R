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






