###################################################
###### CODE VERGL-POLII (WINTER 2020-21) ##########
######### DIPL.-POL. HENNING BERGMANN #############
###################################################
############## FLORIAN WISNIEWSKI #################
####### STUDENT M. A. POLITIKWISSENSCHAFT #########
###################################################

# Jeglicher Code, der für diese Hausarbeit geschrieben wurde, ist im GitHub-Repository (https://github.com/FWisniewski44/CabTerm) für diese Arbeit enthalten.
# Diese Datei listet zudem erneut die endgültig in der Arbeit verwendeten Code-Snippets auf.
# Ebenso sind diese aus der RMarkdown-Datei "HA-word.Rmd" ersichtlich.

################################################### VERWENDETE PACKAGES

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
library(rms)
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
library(gmodels)
library(summarytools)
library(flextable)
library(huxtable)

################################################### VERWENDETE DATENSÄTZE

# load simple erdda df
erdda <- readxl::read_xls(path = "/Users/flo/Desktop/data/erdda/Data ERD-e_SA_SOE_JH_N-29_2014.xls")

# bergmann and erdda, joined df
BergmannERDDA <- read_dta(file = "/Users/flo/Documents/GitHub/CabTerm/erdda2014-Bergmann2019_merged.dta")
BergmannERDDA <- as_tibble(BergmannERDDA)

# bergmann and erdda, filtered and renamed (Datensatz auf Anfrage)
load(file = "BergmannERDDA.f.RData")

################################################### TESTS DER SUBSETS

# STRUKTUR
sbStructure <- subset(BergmannERDDA.f, select = c("discr2019", "abs_dur", "cab_seat_share",
                                                  "num_cabparties", "max_bargpow_cab",
                                                  "max_poss_dur", "effec_parties_parl"))

# model for structural vars
coxPH.STR <- coxph(data = sbStructure, Surv(time = sbStructure$abs_dur, event = sbStructure$discr2019, type = "right") ~ . - abs_dur - discr2019)

logLik(coxPH.STR)
extractAIC(coxPH.STR)
summary(coxPH.STR)
cox.zph(coxPH.STR)
car::vif(coxPH.STR)

# PRÄFERENZEN
sbPreferences <- subset(BergmannERDDA.f, select = c("discr2019", "abs_dur", "pref_range",
                                                    "median_party_cab", "cons_cab", "connected_cab",
                                                    "soc_cab", "parl_pref_range", "antisys_seat"))

# model fit for part II: PREFERENCES 
coxPH.PREF <- coxph(data = sbPreferences, Surv(time = sbPreferences$abs_dur, event = sbPreferences$discr2019, type = "right") ~ . - abs_dur - discr2019)

logLik(coxPH.PREF)
extractAIC(coxPH.PREF)
summary(coxPH.PREF)
cox.zph(coxPH.PREF)
car::vif(coxPH.PREF)

# INSTITUTIONEN
sbInstit <- subset(BergmannERDDA.f, select = c("discr2019", "abs_dur", "invest",
                                               "cabunan", "pm_pow",
                                               "pm_diss_pow", "erdda_bicam", "erdda_semip",
                                               "post_election_cabinet"))

# model fit for part III: INSTITUTIONS
coxPH.INST <- coxph(data = sbInstit, Surv(time = sbInstit$abs_dur, event = sbInstit$discr2019, type = "right") ~ . - abs_dur - discr2019 - pm_pow - pm_diss_pow)

logLik(coxPH.INST)
extractAIC(coxPH.INST)
summary(coxPH.INST)
cox.zph(coxPH.INST)
car::vif(coxPH.INST)

# BARGAINING ENVIRONMENT
sbBarg <- subset(BergmannERDDA.f, select = c("discr2019", "abs_dur", "same_pm", "erdda_barg_dur", "max_poss_dur", "coal_cab"))

# model fit for part IV: BARGAINING 
coxPH.BARG <- coxph(data = sbBarg, Surv(time = sbBarg$abs_dur, event = sbBarg$discr2019, type = "right") ~ . - abs_dur - discr2019)

logLik(coxPH.BARG)
extractAIC(coxPH.BARG)
summary(coxPH.BARG)
cox.zph(coxPH.BARG)
car::vif(coxPH.BARG)

# CRITICAL EVENTS
sbCrit.Abs <- subset(BergmannERDDA.f, select = c("discr2019", "abs_dur", "elect_volat", "inflation_Abs", "unemploy_Abs", "soc_cab", "cons_cab"))

# model fit for part V: CRITICAL EVENTS 
coxPH.CRIT.ABS <- coxph(data = sbCrit.Abs, Surv(time = sbCrit.Abs$abs_dur, event = sbCrit.Abs$discr2019, type = "right") ~ elect_volat + unemploy_Abs)

logLik(coxPH.CRIT.ABS)
extractAIC(coxPH.CRIT.ABS)
summary(coxPH.CRIT.ABS)
cox.zph(coxPH.CRIT.ABS)
# with interaction effects, the ph assumption is not met
# without this, it is very closely met, but nothing is significant

car::vif(coxPH.CRIT.ABS)
# with interactions, VIFs are unnaturally high
# without interactions, VIFs are even higher...



################################################### KREATION BEST FIT

# first step: generate one big subset out of the others
sbAll <- cbind(sbBarg, sbInstit, sbPreferences, sbStructure, sbCrit.Abs)

#this HAS to be executed in this order!
## delete double discr2019 and abs_dur
sbAll[c(7, 8, 16, 17, 25, 26, 32, 33)] <- list(NULL)
## delete other double vars
### max_poss_dur
sbAll[24] <- NULL
### soc_cab and cons_cab
sbAll[c(28, 29)] <- list(NULL)

# get rid of NAs
sbAll <- na.omit(sbAll)

####

# null model and full model
modNull <- coxph(data = sbAll, Surv(time = sbAll$abs_dur, event = sbAll$discr2019, type = "right") ~ 1)
modFull <- coxph(data = sbAll, Surv(time = sbAll$abs_dur, event = sbAll$discr2019, type = "right") ~ . -abs_dur - discr2019)

# step function for acquiring best fit
# stepAIC(modNull, scope = list(upper = modFull), direction = "both")

# best fit as object with summary and test for ph assumption
# best fit as object with summary and test for ph assumption
best.fit <- coxph(formula = Surv(time = sbAll$abs_dur, event = sbAll$discr2019, type = "right") ~ 
                    erdda_bicam + effec_parties_parl + parl_pref_range + 
                    post_election_cabinet + erdda_barg_dur + same_pm + cab_seat_share, data = sbAll)

logLik(best.fit)
extractAIC(best.fit)
summary(best.fit)
cox.zph(best.fit)
car::vif(best.fit)

# TABELLE BEST FIT
tidyBestFit <- (tidy(best.fit, exponentiate = T, conf.level = 0.95)) %>% 
  mutate(signif = stars.pval(p.value))

kbl(tidyBestFit) %>% 
  kable_classic()

################################################### ABBILDUNGEN

# LÄNDER MIT MINDERHEITSREGIERUNGEN
ht1 <- huxtable(freq(BergmannERDDA.f$country), autoformat = T, add_rownames = T)
add_colnames(ht1)

# MINDERHEITSREGIERUNGEN: ANTEIL IN DEN DATEN
add_colnames(huxtable(freq(BergmannERDDA$v329e), add_rownames = T, autoformat = T))

# BASELINE-GRAPH
plot(survfit(Surv(time = BergmannERDDA.f$abs_dur, 
                  event = BergmannERDDA.f$discr2019, type = "right")~1))

# KORRELATIONEN STRUKTUR (D3HEATMAP = INTERAKTIVE MATRIX)
corStructure <- round(cor(na.omit(sbStructure)), 2)

corrplot(corStructure, type = "upper", method = "number", order = "hclust")
# d3heatmap::d3heatmap(corStructure, Rowv = FALSE, Colv=FALSE)

# KORRELATIONEN PRÄFERENZEN (D3HEATMAP = INTERAKTIVE MATRIX)
corPreferences <- round(cor(na.omit(sbPreferences)), 2)

# heatmap
corrplot(corPreferences, type = "upper", method = "number", order = "hclust")
# d3heatmap::d3heatmap(corPreferences, Rowv = FALSE, Colv=FALSE)

# KORRELATIONEN INSTITUTIONEN (D3HEATMAP = INTERAKTIVE MATRIX)
corInstit <- round(cor(na.omit(sbInstit)), 2)

# heatmaps
corrplot(corInstit, type = "upper", method = "number", order = "hclust")
# d3heatmap::d3heatmap(corInstit, Rowv = FALSE, Colv = FALSE)

# KORRELATIONEN BARGAINING (D3HEATMAP = INTERAKTIVE MATRIX)
corBarg <- round(cor(na.omit(sbBarg)), 2)

corrplot(corBarg, type = "upper", method = "number", order = "hclust")
# d3heatmap::d3heatmap(corBarg, Rowv = FALSE, Colv=FALSE)

# KORRELATIONEN CRITICAL EVENTS (D3HEATMAP = INTERAKTIVE MATRIX)
corCrit.Abs <- round(cor(na.omit(sbCrit.Abs)), 2)

# heatmaps
corrplot(corCrit.Abs, type = "upper", method = "number", order = "hclust")

# KORRELATIONEN BEST FIT
corAll <- round(cor(na.omit(sbAll)), 2)

corrplot(corAll, type = "upper", method = "circle", order = "hclust")

# SURVIVAL PLOT, NACH GERECHNETEM MODELL
ggsurvplot(data = sbAll, survfit(best.fit), conf.int = F, linetype = "solid", 
           risk.table = "abs_pct", 
           surv.median.line = "hv",
           tables.theme = theme_minimal(),
           ggtheme = theme_linedraw()) + 
  ggtitle(label = "Überlebenswahrscheinlichkeit von Regierungen",
          subtitle = "geschätzt durch Modell des best fit")

# ENDE DES DOKUMENTS






