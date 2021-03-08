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

########################################### models test with safely merged df

BergmannERDDA <- read_dta(file = "/Users/flo/Documents/GitHub/CabTerm/erdda2014-Bergmann2019_merged.dta")
BergmannERDDA <- as_tibble(mergeDF)

fre(BergmannERDDA$discr2019)
# test
fre(combo$discr2019)

########################################### 

BergmannERDDA$v329e

# this: (v329e == 1 is minority government) --> we get 207 cases of minority cabs
BergmannERDDA %>% filter(v329e == 1)
combo %>% filter(v329e == 1)

# or this (filter by minor_coalition and one_party_minor) --> we geht cases of minority cabs
BergmannERDDA %>% filter(minor_coalition == 1 | one_party_minor == 1)
combo %>% filter(minor_coalition == 1 | one_party_minor == 1)

########################################### 






