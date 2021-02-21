# libraries survival analysis
library(survival)
library(survminer)
library(survMisc)
library(simPH)
library(flexsurv)

# libraries general
library(tidymodels)
library(tidyverse)
library(lubridate)
library(Amelia)
library(xtable)
library(haven)
library(broom)
library(psych)
library(Hmisc)
library(expss)
library(rockchalk)
library(effects)
library(modeldata)
library(randomForest)

###########################################
# import relevant dfs
## import dta bergmann df
dtaBerg <- as_tibble(read_dta(file = "/Users/flo/Documents/Uni/Uni Bamberg/WS 2020-21/S Koalitionsforschung/data/cabinets_2019_Bergmann.dta"))
str(dtaBerg)
# View(dtaBerg)
head(dtaBerg)

## import xls ERDDA df
erdda <- as_tibble(readxl::read_excel(path = "/Users/flo/Desktop/data/erdda/Data ERD-e_SA_SOE_JH_N-29_2014.xls"))
str(erdda)
# View(erdda)
head(erdda)

###########################################
attach(dtaBerg)

# first manipulations
fre(discr2019)
fre(techn2019)
fre(early2019)
fre(repl2019)

## comment: in discr2019, 0 == "other" and 1 == "discretionary termination"
## value for 0 == 299; value for 1 == 453 - that said: of 752 cabinets terminated,
## 299 due to technical reasons, 453 discretionary terminations (either early elections or replacements).


# what is surv-object?
fre(date_out_yr)
fre(date_in_yr)
fre(abs_dur)
max(abs_dur)

## need abs_dur, because absolute duration in days of cabinet reign

help("Surv")
#
firstSurv <- Surv(abs_dur, event = discr2019)
#
secondSurv <- Surv(abs_dur, event = early2019)
#
thirdSurv <- Surv(abs_dur, event = repl2019)


# base syntax coxph
help("coxph")

## kaplan meier model - discr19
kmModDiscr <- survfit(firstSurv ~ 1)
summary(kmModDiscr)
## kaplan meier plot - discr19
plot(kmModDiscr, conf.int = F, xlab = "Time of Survival (in days)", ylab = "% of Cabinets 'alive'",
     main = "Kaplan-Meier-Plot, pooled")

## kaplan meier model - EARLY19
kmEarly <- survfit(secondSurv ~ 1)
summary(kmEarly)
## kaplan meier plot - EARLY19
plot(kmEarly, conf.int = F, xlab = "Time of Survival (in days)", ylab = "% of Cabinets 'alive'",
     main = "Kaplan-Meier-Plot, Early Elections")

## kaplan meier model - REPL19
kmRepl <- survfit(thirdSurv ~ 1)
summary(kmRepl)
## kaplan meier plot - REPL19
plot(kmRepl, conf.int = F, xlab = "Time of Survival (in days)", ylab = "% of Cabinets 'alive'",
     main = "Kaplan-Meier-Plot, Replacements")


## test model
summary(coxph(data = dtaBerg, firstSurv ~ major_cabinet))




