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
library(foreign)
library(broom)
library(psych)
library(Hmisc)
library(expss)
library(rockchalk)
library(effects)
library(modeldata)
library(randomForest)

########################################### import relevant dfs
# import dta bergmann df
dtaBerg <- as_tibble(read_dta(file = "/Users/flo/Documents/Uni/Uni Bamberg/WS 2020-21/S Koalitionsforschung/data/cabinets_2019_Bergmann.dta"))
str(dtaBerg)
### View(dtaBerg)
head(dtaBerg)

# import xls ERDDA df
erdda <- as_tibble(readxl::read_excel(path = "/Users/flo/Desktop/data/erdda/Data ERD-e_SA_SOE_JH_N-29_2014.xls"))
str(erdda)
### View(erdda)
head(erdda)

########################################### get a grip of available data
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


# test for full model - syntax reference
summary(coxph(data = dtaBerg, firstSurv ~ minor_coalition))



########################################### filter df for relevant vars
# vars for minority coalition cabinet - minor_coalition and one_party_minor
fre(minor_coalition)
## in this case 0 == no; 1 == yes
## there are 113 minority cabinets in the df

fre(one_party_minor)
## coding equal to minor_coalition

# var for post election cabinet
fre(post_election_cabinet)
## study to include only those governments that came to power through regular elections
## at the end of respective national legislative periods
## codes as 1 == yes; 0 == no

# filter df for minority coalitions (one_party_minor | minor_coalition)
filterBerg <- filter(dtaBerg, post_election_cabinet == 1 & minor_coalition == 1 | one_party_minor == 1)
View(filterBerg)
save(filterBerg, file = "filterBergmann.RData")
## the df is filtered for post election cabinets == T AND minority coalition governments OR
## one party minority governments == T


# new Surv-item with filterBerg
Surv(filterBerg$abs_dur, event = filterBerg$discr2019)


# fit test model for this df
testModel <- coxph(data = filterBerg, Surv(filterBerg$abs_dur, event = filterBerg$discr2019) ~ filterBerg$eff_numb_parties + filterBerg$cab_barg_duration)
summary(testModel)
## coef here is showing hazard RATIOS

# test: proportional hazards assumption met?
cox.zph(testModel)
ggcoxzph(cox.zph(testModel))
## when significant values at vars or on global level: PH assumption is NOT met! Has to be insignificant!
## graphically: Schoenfeld test - there must not be a pattern of shifting values at any given point in time!


###########################################  vars to include?
# dependent var and miscellaneous/other

## discr2019 --- discretionary cabinet terminations (2019)
### code: 1 == discretionary ; 0 == others

## for event history analysis/data over time: abs_dur --- absolute duration (days)

## filtered vars are one_party_minor and minor_coalition (filter for minority cabinets only)
## and post_election_cabinet (cabinets after elections at the end of legislative periods)

# independent vars (based on Saalfeld 2008)

## eff_numb_parties --- effective number of parties
### based on Laakso/Taagepera index (1979)

## cab_barg_duration --- cabinet bargaining duration
### measured in days

## pm_diss_pow --- Prime Minister dissolution powers
### index from 0-10 based on Goplerud/Schleiter 2016
### additionally: look up what Saalfeld was using in 2008

## rl-range --- overall parliamentary RILE range
### range = maximum - minimum; higher values = more RILE polarization
### this is for the parliament as a whole - is another polarization var needed just for the cab?
#### possibility to merge ERDDA variable for this into dtaBerg?

## polarization_bpw --- polarization (bp weighted)
### polarization of cabinets, value as calculated in ERDDA 2014, hence also only data for countries from ERDDA
### rather include this for cabinet preference range?

## 

## building a subset with said vars
detach(dtaBerg)
attach(filterBerg)
subset(data = filterBerg, select = c(discr2019, abs_dur, one_party_minor, minor_coalition, 
                                     post_election_cabinet, eff_numb_parties, cab_barg_duration,
                                     pm_diss_pow, polarization_bpw, rl_range))


###########################################  merge dfs for some reasons?

# include cabinet preference range from ERDDA into dtaBergmann

## find proper function for merging - merge or cbind?
### cannot use cbind because structure is not entirely the same! This would cause the values to end up
### in wrong locations - therefore merge has to be used

# get some overview
head(dtaBerg)
head(erdda)
all_equal(dtaBerg, erdda)
all_equal(erdda, BergErdda)

### this just shows different number of columns (158 vs 202) - is the rest the same df?

# id vars are erdda$v001e and dtaBerg
# try merging by "country"
help(merge)
merge(dtaBerg, erdda, by.x = "country", by.y = "v001e")
merge(dtaBerg, erdda, by = NULL)

BergErdda <- merge(dtaBerg, erdda, by.x = "country", by.y = "v001e", all = T)
fre(BergErdda$v106e)
fre(erdda$v106e)

### this does not really work (apparently...): ERDDA values are not taken over to BergErdda
#### correction: works - had to specify all = T, so that merge() can create NAs when needed

fre(BergErdda$v407e)
fre(erdda$v407e)




