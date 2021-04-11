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
library(ggpubr)
library(pander)
library(knitr)
library(huxtable)

# some graphical stuff

freq(BergmannERDDA.f$country)

kable(freq(BergmannERDDA.f$country), )

huxtable(freq(BergmannERDDA.f$country), add_rownames = T, add_colnames = TRUE)

# table for best fit
tidyBestFit <- (tidy(best.fit, exponentiate = T, conf.level = 0.95)) %>% 
  mutate(signif = stars.pval(p.value))

kbl(tidyBestFit) %>% 
  kable_classic()

# estimated survival by best fitting model

ggsurvplot(data = sbAll, survfit(best.fit), conf.int = F, linetype = "solid", 
           risk.table = "abs_pct", 
           surv.median.line = "hv",
           tables.theme = theme_minimal(),
           ggtheme = theme_linedraw()) + 
  ggtitle(label = "Überlebenswahrscheinlichkeit von Regierungen",
          subtitle = "geschätzt durch Modell des best fit")







