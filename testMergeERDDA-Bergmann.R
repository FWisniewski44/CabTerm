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

# Test: merge Bergmann and ERDDA (once again)

# import dta bergmann df
berg <- as_tibble(readxl::read_xls(path = "/Users/flo/Desktop/data/bergmann2019/cabinets_2019_Bergmann.xls"))

# import erdda df
erdda <- as_tibble(readxl::read_xls(path = "/Users/flo/Desktop/data/erdda/Data ERD-e_SA_SOE_JH_N-29_2014.xls"))


###########################################


# recode cab_code for merge in erdda
erdda$cab_code <- erdda$v002e

# joining dfs
combo <- inner_join(erdda, berg, by = "cab_code")
# comboMerge <- merge(erdda, berg, by = "cab_code")
# i double-checked this in stata and it worked, so inner_join == merge 1:1 in stata
# save(combo, file = "combo.RData")

# see which cases are dropped
# antiCombo <- anti_join(erdda, berg, by = "cab_code")


# testing
fre(combo$discr2019)
fre(berg$discr2019)

fre(combo$v329e)
fre(erdda$v329e)

fre(combo$minor_coalition)
fre(combo$one_party_minor)

fre(berg$minor_coalition)
fre(berg$one_party_minor)

# filter

## comboMinority <- combo %>% filter(v329e == 1)
## combo %>% filter(minor_coalition == 1 | one_party_minor == 1)

erddaMinor <- erdda %>% filter(v329e == 1)
bergMinor <- berg %>%  filter(minor_coalition == 1 | one_party_minor == 1)

comboMinority <- inner_join(erddaMinor, bergMinor, by = "cab_code", keep = T)

# check whats not merged
anti_join(erddaMinor, bergMinor, by = "cab_code", keep = T)

###########################################

# EOD

