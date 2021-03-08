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

###################################

# load dfs

dtaBerg <- as_tibble(read_dta(file = "/Users/flo/Documents/Uni/Uni Bamberg/WS 2020-21/S Koalitionsforschung/data/cabinets_2019_Bergmann.dta"))

erdda <- as_tibble(readxl::read_excel(path = "/Users/flo/Desktop/data/erdda/Data ERD-e_SA_SOE_JH_N-29_2014.xls"))

# test: filter just for minority cabs
Fil1 <- filter(dtaBerg, dtaBerg$one_party_minor == 1)
Fil2 <- filter(dtaBerg, dtaBerg$minor_coalition == 1)

fre(Fil1$discr2019)
# yes == 1 (79 obs), no == 0 (48 obs)

fre(Fil2$discr2019)
# yes == 1 (76 obs); no == 0 (37 obs)

###############################

fil.full <- full_join(Fil1, Fil2, by = NULL)

fil.full <- fil.full %>% filter(techn2019 == 0)

########

# surv object
Surv(fil.full$abs_dur, event = fil.full$discr2019)

# STRUCTURE
summary(coxph(data = fil.full, Surv(fil.full$abs_dur, event = fil.full$discr2019) ~ post_election_cabinet + num_cabparties + max_dur + eff_numb_parties))

# PREFERENCES
summary(coxph(data = fil.full, Surv(fil.full$abs_dur, event = fil.full$discr2019) ~ rl_polar + right_seat + comm_seat + rl_range + polarization_bpw + antisys_seat))

# INSTITUTIONS
summary(coxph(data = fil.full, Surv(fil.full$abs_dur, event = fil.full$discr2019) ~ positive_parl + pm_diss_pow + bicameralism + semi_presidentialism))

# BARGAINING
summary(coxph(data = fil.full, Surv(fil.full$abs_dur, event = fil.full$discr2019) ~ same_pm + cab_barg_duration + max_dur))









