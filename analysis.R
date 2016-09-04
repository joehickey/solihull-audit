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
 
# Check non-parametric correlations?
# Add school in a multilevel model of SKCQ change? - Check Field p.878 on how.
# For hypothesis 3, non-parametric or robust alternative to the pre/post regression 
# already calculated in 'analysis'.

### HYPOTHESIS 1: Confidence with emotional/behavioural difficulties, understanding of Solihull concepts and intent to use Solihull concepts all increase following Solihull training

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


### HYPOTHESIS 2: Professional quality of life increases following Solihull training

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


### HYPOTHESIS 3: Solihull training effects predict increased professional quality of life

# Test for linear relationships between change in SCKQ and change in PROQOL
rcorr(as.matrix(data.frame(SKCQAll, proQolAll), type = "pearson"))
# Significant correlations between SKCQ2Use and proQolPostCS, SKCQ2Use and proQolPostBO
# Others?

# Correlate change scores for SKCQ and PROQOL variables
ChgScores <-
  data.frame(SKCQConfChg,
             SKCQUndChg,
             SKCQUseChg,
             proQolCSChg,
             proQolBOChg,
             proQolSTSChg)
rcorr(as.matrix(ChgScores), type = "pearson")
# No significant correlations of change scores

# Regressions testing whether follow-up PROQOL variables are predicted by SKCQ training effects
fit <-
  lm(proQolPostCS ~ proQolPreCS + SKCQConfChg + SKCQUndChg + SKCQUseChg,
     data = dat)
summary(fit)
fit <-
  lm(proQolPostBO ~ proQolPreBO + SKCQConfChg + SKCQUndChg + SKCQUseChg,
     data = dat)
summary(fit)
fit <-
  lm(proQolPostSTS ~ proQolPreSTS + SKCQConfChg + SKCQUndChg + SKCQUseChg,
     data = dat)
summary(fit)
# Change in understanding of Solihull concepts is a signifcant predictor of STS at follow-up

# datSchOnly <- dat[dat$school %in% c(2:5), ] # Works to select only school-based trainings

### HYPOTHESIS 3 partially accepted