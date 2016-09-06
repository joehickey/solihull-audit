# Set working directory
setwd("~/Google Drive/psychology/cyp/solihull/data/R") # Amend as needed
library(ggplot2)
library(gplots)
library(car)
library(tidyr)
library(plyr)
library(nlme)
library(multcomp)
library(dplyr)
library(psy)
library (Hmisc)

# ToDo:
# If using parametric stats, maybe add school in a multilevel model of SKCQ change? - Field p.878
# If using parametric stats, finish properly specifying and checking regressions for H3.

##################    Confidence with emotional/behavioural difficulties,
## HYPOTHESIS 1 ##    understanding of Solihull concepts
##################    and intent to use Solihull concepts all increase following Solihull training

# Non-parametric tests
wilcox.test(
  SKCQ1Conf,
  SKCQ4Conf,
  alternative = "two.sided",
  paired = TRUE,
  exact = FALSE,
  conf.int = TRUE
)

# Parametric tests
t.test(SKCQ1Conf, SKCQ4Conf, paired = T)
t.test(SKCQ1Und, SKCQ4Und, paired = T)
t.test(SKCQ1Use, SKCQ4Use, paired = T)

# Mixed model of Solihull training effects over time
# Confidence
contrasts(datLong$timepoint) <- contr.helmert(4)
confModel <-
  lme(
    solihullConf ~ timepoint,
    random = ~ 1 |
      caseID / timepoint,
    data = datLong,
    method = "ML",
    na.action = na.omit
  )
summary(confModel)
baseline <-
  lme(
    solihullConf ~ 1,
    random = ~ 1 |
      caseID / timepoint,
    data = datLong,
    method = "ML",
    na.action = na.omit
  )
anova(baseline, confModel)
postHocs <- glht(confModel, linfct = mcp(timepoint = "Tukey"))
summary(postHocs)

confModelAge <-
  update(confModel, .~. + age)

# Understanding
undModel <-
  lme(
    solihullUnd ~ timepoint,
    random = ~ 1 |
      caseID / timepoint,
    data = datLong,
    method = "ML",
    na.action = na.omit
  )
summary(undModel)
baseline <-
  lme(
    solihullUnd ~ 1,
    random = ~ 1 |
      caseID / timepoint,
    data = datLong,
    method = "ML",
    na.action = na.omit
  )
anova(baseline, undModel)
postHocs <- glht(undModel, linfct = mcp(timepoint = "Tukey"))
summary(postHocs)
# Using
useModel <-
  lme(
    solihullUse ~ timepoint,
    random = ~ 1 |
      caseID / timepoint,
    data = datLong,
    method = "ML",
    na.action = na.omit
  )
summary(useModel)
baseline <-
  lme(
    solihullUse ~ 1,
    random = ~ 1 |
      caseID / timepoint,
    data = datLong,
    method = "ML",
    na.action = na.omit
  )
anova(baseline, useModel)
postHocs <- glht(useModel, linfct = mcp(timepoint = "Tukey"))
summary(postHocs)

### HYPOTHESIS 1 accepted...?

##################
## HYPOTHESIS 2 ## Professional quality of life increases following Solihull training
##################

# Non-parametric tests
wilcox.test(
  proQolPreCS,
  proQolPostCS,
  alternative = "two.sided",
  paired = TRUE,
  exact = FALSE,
  conf.int = TRUE
)

# Parametric tests
t.test(proQolPreCS, proQolPostCS, paired = T)
t.test(proQolPreBO, proQolPostBO, paired = T)
t.test(proQolPreSTS, proQolPostSTS, paired = T)

### HYPOTHESIS 2 accepted

###################
### HYPOTHESIS 3 ## Solihull training effects predict increased professional quality of life
###################

# Test for linear relationships between change in SCKQ and change in PROQOL
rcorr(as.matrix(data.frame(SKCQAll, proQolAll), type = "spearman"))
# Follow up significant correlations
cor.test(SKCQ2Use, proQolPostCS, method = "spearman")
cor.test(SKCQ2Use, proQolPostBO, method = "spearman")
# Significant correlations between SKCQ2Use and proQolPostCS, SKCQ2Use and proQolPostBO
# What about controlling for baseline PROQOL?
# Correlate change scores for SKCQ and PROQOL variables
ChgScores <-
  data.frame(dat$SKCQConfChg,
             dat$SKCQUndChg,
             dat$SKCQUseChg,
             dat$proQolCSChg,
             dat$proQolBOChg,
             dat$proQolSTSChg)
rcorr(as.matrix(ChgScores), type = "spearman")
# No significant correlations of change scores

# Regressions testing whether follow-up PROQOL variables are predicted by SKCQ training effects
fitReg1 <-
  lm(proQolPostCS ~ proQolPreCS + SKCQConfChg + SKCQUndChg + SKCQUseChg,
     data = dat)
summary(fitReg1) # No significant main effects of SKCQ changes
fitReg2 <-
  lm(proQolPostBO ~ proQolPreBO + SKCQConfChg + SKCQUndChg + SKCQUseChg,
     data = dat)
summary(fitReg2) # No significant main effects of SKCQ changes
fitReg3 <-
  lm(proQolPostSTS ~ proQolPreSTS + SKCQConfChg + SKCQUndChg + SKCQUseChg,
     data = dat)
summary(fitReg3) # Change in understanding of Solihull concepts is a signifcant predictor of STS at follow-up


### HYPOTHESIS 3 partially accepted
