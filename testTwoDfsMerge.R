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
library(compare)
library(psych)
library(desc)

########################################### import relevant dfs
# import dta bergmann df
dtaBerg <- as_tibble(read_dta(file = "/Users/flo/Documents/Uni/Uni Bamberg/WS 2020-21/S Koalitionsforschung/data/cabinets_2019_Bergmann.dta"))

# dtaBerg linux:
dtaBergL <- as_tibble(read_dta(file = "/home/fw/Downloads/cabinets_2019_Bergmann.dta"))

### str(dtaBerg)
### View(dtaBerg)
head(dtaBerg)

# import xls ERDDA df
erdda <- as_tibble(readxl::read_excel(path = "/Users/flo/Desktop/data/erdda/Data ERD-e_SA_SOE_JH_N-29_2014.xls"))

# erdda linux:
erddaL <- as_tibble(readxl::read_excel(path = "/home/fw/Downloads/Data ERD-e_SA_SOE_JH_N-29_2014.xls"))

### str(erdda)
### View(erdda)
head(erdda)




########################################### filter

berg1PartyMin <- filter(dtaBergL, dtaBergL$one_party_minor == 1 & dtaBergL$post_election_cabinet == 1)
bergCabMin <- filter(dtaBergL, dtaBergL$minor_coalition == 1 & dtaBergL$post_election_cabinet == 1)
fre(bergCabMin$discr2019)
# yes == 1 (33 obs), no == 0 (10 obs)

fre(berg1PartyMin$discr2019)
# yes == 1 (49 obs); no == 0 (23 obs)

########################################### df joins

########## join via cab_code, as it is the same in both, of course
BerJoi <- full_join(berg1PartyMin, bergCabMin, by = NULL)
fre(BerJoi$discr2019)

##########
# well, it seems to work now...
# see:
## yes == 1 (33 obs); no == 0 (82 obs)
## just like i wanted it to work: adds the numbers up
















