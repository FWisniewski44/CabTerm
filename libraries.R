# libraries survival analysis
library(survival)
library(survminer)
library(survMisc)
library(simPH)
library(flexsurv)

# libraries general
library(tidymodels)
library(tidyverse)
library(xtable)
library(haven)
library(broom)
library(psych)
library(Hmisc)
library(expss)

###########################################
# import relevant dfs
## import dta bergmann df
dtaBerg <- read_dta(file = "/Users/flo/Documents/Uni/Uni Bamberg/WS 2020-21/S Koalitionsforschung/data/cabinets_2019_Bergmann.dta")
str(dtaBerg)
View(dtaBerg)
head(dtaBerg)

## import xls ERDDA df
erdda <- readxl::read_excel(path = "/Users/flo/Documents/Uni/Uni Bamberg/WS 2020-21/S Koalitionsforschung/data/Data ERD-e_SA_SOE_JH_N-29_2014.xls")
str(erdda)
View(erdda)
head(erdda)



