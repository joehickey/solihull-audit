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
library(gridExtra)
library(psych)
library(pastecs)
library(Hmisc)

# toDo:
# Check in multivariate EDA whether subgroups anticipated to be controlled are individually normal, 
# i.e. school staff vs others. Also check by timepoint for DVs.
# Calculate Levene's test of homogoneity of variances in multivariate EDA for factors, and graphs (Field 7.9.5)
# for correlational analysis.
# Add in the relevant training feedback variables

#################
## Examine IVs ##
#################

indVars <-
  cbind(dat$age, dat$timeAtSchool, dat$timeInJob, deparse.level = 2)

round(stat.desc(indVars, basic = FALSE, norm = TRUE), digits = 3)
# Indicates significant skewness and kurtosis for all three variables

# Check distribution of age
shapiro.test(dat$age)
ggplot(dat, aes(age)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  stat_function(
    fun = dnorm,
    args = list(
      mean = mean(dat$age, na.rm = TRUE),
      sd = sd(dat$age, na.rm = TRUE)
    ),
    colour = "black",
    size = 1
  ) # Cannot assume normality

dat$ageSqrt <- 
  sqrt(dat$age) # Try square root transformation

shapiro.test(dat$ageSqrt) # Check again
ggplot(dat, aes(ageSqrt)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  stat_function(
    fun = dnorm,
    args = list(
      mean = mean(dat$ageSqrt, na.rm = TRUE),
      sd = sd(dat$ageSqrt, na.rm = TRUE)
    ),
    colour = "black",
    size = 1
  ) # Cannot assume normality

dat$ageLog <- 
  log(dat$age) # Try log transformation

shapiro.test(dat$ageLog) # Check again
ggplot(dat, aes(ageLog)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  stat_function(
    fun = dnorm,
    args = list(
      mean = mean(dat$ageLog, na.rm = TRUE),
      sd = sd(dat$ageLog, na.rm = TRUE)
    ),
    colour = "black",
    size = 1
  ) # Cannot assume normality

dat$ageRecip <- 
  1 / (dat$age) # Try reciprocal transformation

shapiro.test(dat$ageRecip) # Check again
ggplot(dat, aes(ageRecip)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  stat_function(
    fun = dnorm,
    args = list(
      mean = mean(dat$ageRecip, na.rm = TRUE),
      sd = sd(dat$ageRecip, na.rm = TRUE)
    ),
    colour = "black",
    size = 1
  ) # Cannot assume normality
# Age not normally distributed and can't be usefully transformed

# Check distribution of timeAtSchool
shapiro.test(dat$timeAtSchool)
ggplot(dat, aes(timeAtSchool)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  stat_function(
    fun = dnorm,
    args = list(
      mean = mean(dat$timeAtSchool, na.rm = TRUE),
      sd = sd(dat$timeAtSchool, na.rm = TRUE)
    ),
    colour = "black",
    size = 1
  ) # Cannot assume normality, strong positive skew

dat$timeAtSchoolSqrt <-
  sqrt(dat$timeAtSchool) # Try square root transformation

shapiro.test(dat$timeAtSchoolSqrt) # Check again
ggplot(dat, aes(timeAtSchoolSqrt)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  stat_function(
    fun = dnorm,
    args = list(
      mean = mean(dat$timeAtSchoolSqrt, na.rm = TRUE),
      sd = sd(dat$timeAtSchoolSqrt, na.rm = TRUE)
    ),
    colour = "black",
    size = 1
  ) # Cannot assume normality

dat$timeAtSchoolLog <-
  log(dat$timeAtSchool + 1) # Try log transformation

shapiro.test(dat$timeAtSchoolLog) # Check again
ggplot(dat, aes(timeAtSchoolLog)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  stat_function(
    fun = dnorm,
    args = list(
      mean = mean(dat$timeAtSchoolLog, na.rm = TRUE),
      sd = sd(dat$timeAtSchoolLog, na.rm = TRUE)
    ),
    colour = "black",
    size = 1
  ) # Cannot assume normality

dat$timeAtSchoolRecip <-
  (1 / (dat$timeAtSchool + 1)) # Try reciprocal transformation

shapiro.test(dat$timeAtSchoolRecip) # Check again
ggplot(dat, aes(timeAtSchoolRecip)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  stat_function(
    fun = dnorm,
    args = list(
      mean = mean(dat$timeAtSchoolRecip, na.rm = TRUE),
      sd = sd(dat$timeAtSchoolRecip, na.rm = TRUE)
    ),
    colour = "black",
    size = 1
  ) # Worse
# timeAtSchool is not nomally distributed and can't be usefully transformed

# Check distribution of timeInJob
shapiro.test(dat$timeInJob)
ggplot(dat, aes(timeInJob)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  stat_function(
    fun = dnorm,
    args = list(
      mean = mean(dat$timeInJob, na.rm = TRUE),
      sd = sd(dat$timeInJob, na.rm = TRUE)
    ),
    colour = "black",
    size = 1
  ) # Cannot assume normality, strong positive skew

dat$timeInJobSqrt <-
  sqrt(dat$timeInJob) # Try square root transformation

shapiro.test(dat$timeInJobSqrt) # Check again
ggplot(dat, aes(timeInJobSqrt)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  stat_function(
    fun = dnorm,
    args = list(
      mean = mean(dat$timeInJobSqrt, na.rm = TRUE),
      sd = sd(dat$timeInJobSqrt, na.rm = TRUE)
    ),
    colour = "black",
    size = 1
  ) # Cannot assume normality

dat$timeInJobLog <- log(dat$timeInJob + 1) # Try log transformation
shapiro.test(dat$timeInJobLog)

ggplot(dat, aes(timeInJobLog)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  stat_function(
    fun = dnorm,
    args = list(
      mean = mean(dat$timeInJobLog, na.rm = TRUE),
      sd = sd(dat$timeInJobLog, na.rm = TRUE)
    ),
    colour = "black",
    size = 1
  ) # Better but still negative skew

dat$timeInJobRecip <-
  (1 / (dat$timeInJob + 1)) # Try reciprocal transformation

shapiro.test(dat$timeInJobRecip) # Check again
ggplot(dat, aes(timeInJobRecip)) +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  stat_function(
    fun = dnorm,
    args = list(
      mean = mean(dat$timeInJobRecip, na.rm = TRUE),
      sd = sd(dat$timeInJobRecip, na.rm = TRUE)
    ),
    colour = "black",
    size = 1
  ) # Much worse
# timeInJob is not nomally distributed but improved by log transformation

# IV Conclusions
# Univariate normality cannot be assumed for age, timeInSchool and timeInJob
# Transformations don't help very much
# Need to use non-parametric or robust tests when including these control variables in analysis

#################
## Examine DVs ##
#################

# NB can we collapse across timepoints to check normality from 'long format' dataset
# instead of appraoch below?

# Check normality for Professional Quality of Life Scale variables
round(stat.desc(proQolAll, basic = FALSE, norm = TRUE), 3)
# Some skewness for proQolPreSTS and proQolPostSTS, check histograms

proQolBW <- 1 # Set binwidth

# Make histograms
proQolPlot1 <-
  ggplot(dat, aes(proQolPreCS)) +
  geom_histogram(
    aes(y = ..density..),
    colour = "black",
    fill = "white",
    binwidth = proQolBW
  ) +
  stat_function(
    fun = dnorm,
    args = list(
      mean = mean(proQolPreCS, na.rm = TRUE),
      sd = sd(proQolPreCS, na.rm = TRUE)
    ),
    colour = "black",
    size = 1
  )
proQolPlot2 <-
  ggplot(dat, aes(proQolPreBO)) +
  geom_histogram(
    aes(y = ..density..),
    colour = "black",
    fill = "white",
    binwidth = proQolBW
  ) +
  stat_function(
    fun = dnorm,
    args = list(
      mean = mean(proQolPreBO, na.rm = TRUE),
      sd = sd(proQolPreBO, na.rm = TRUE)
    ),
    colour = "black",
    size = 1
  )
proQolPlot3 <-
  ggplot(dat, aes(proQolPreSTS)) +
  geom_histogram(
    aes(y = ..density..),
    colour = "black",
    fill = "white",
    binwidth = proQolBW
  ) +
  stat_function(
    fun = dnorm,
    args = list(
      mean = mean(proQolPreSTS, na.rm = TRUE),
      sd = sd(proQolPreSTS, na.rm = TRUE)
    ),
    colour = "black",
    size = 1
  )
proQolPlot4 <-
  ggplot(dat, aes(proQolPostCS)) +
  geom_histogram(
    aes(y = ..density..),
    colour = "black",
    fill = "white",
    binwidth = proQolBW
  ) +
  stat_function(
    fun = dnorm,
    args = list(
      mean = mean(proQolPostCS, na.rm = TRUE),
      sd = sd(proQolPostCS, na.rm = TRUE)
    ),
    colour = "black",
    size = 1
  )
proQolPlot5 <-
  ggplot(dat, aes(proQolPostBO)) +
  geom_histogram(
    aes(y = ..density..),
    colour = "black",
    fill = "white",
    binwidth = proQolBW
  ) +
  stat_function(
    fun = dnorm,
    args = list(
      mean = mean(proQolPostBO, na.rm = TRUE),
      sd = sd(proQolPostBO, na.rm = TRUE)
    ),
    colour = "black",
    size = 1
  )
proQolPlot6 <-
  ggplot(dat, aes(proQolPostSTS)) +
  geom_histogram(
    aes(y = ..density..),
    colour = "black",
    fill = "white",
    binwidth = proQolBW
  ) +
  stat_function(
    fun = dnorm,
    args = list(
      mean = mean(proQolPostSTS, na.rm = TRUE),
      sd = sd(proQolPostSTS, na.rm = TRUE)
    ),
    colour = "black",
    size = 1
  )
grid.arrange(proQolPlot1,
             proQolPlot2,
             proQolPlot3,
             proQolPlot4,
             proQolPlot5,
             proQolPlot6,
             ncol = 3) # Display histograms

# Accept normal distributions for PROQOL variables for now, need
# to check when combining across pre and post timepoints


# Check normality for SKCQ variables
round(stat.desc(
  cbind(
    datLong$solihullConf,
    datLong$solihullUnd,
    datLong$solihullUse,
    deparse.level = 2
  ),
  basic = FALSE,
  norm = TRUE
), 3)
# Significant negative skewness for all three varialbes
# Check histograms
SKCQLongBW <- 0.3 # Set the bin width
SKCQLongPlot1 <-
  ggplot(datLong, aes(datLong$solihullConf)) +
  geom_histogram(
    aes(y = ..density..),
    colour = "black",
    fill = "white",
    binwidth = SKCQLongBW
  ) +
  stat_function(
    fun = dnorm,
    args = list(
      mean = mean(datLong$solihullConf, na.rm = TRUE),
      sd = sd(datLong$solihullConf, na.rm = TRUE)
    ),
    colour = "black",
    size = 1
  )
SKCQLongPlot2 <-
  ggplot(datLong, aes(datLong$solihullUnd)) +
  geom_histogram(
    aes(y = ..density..),
    colour = "black",
    fill = "white",
    binwidth = SKCQLongBW
  ) +
  stat_function(
    fun = dnorm,
    args = list(
      mean = mean(datLong$solihullUnd, na.rm = TRUE),
      sd = sd(datLong$solihullUnd, na.rm = TRUE)
    ),
    colour = "black",
    size = 1
  )
SKCQLongPlot3 <-
  ggplot(datLong, aes(datLong$solihullUse)) +
  geom_histogram(
    aes(y = ..density..),
    colour = "black",
    fill = "white",
    binwidth = SKCQLongBW
  ) +
  stat_function(
    fun = dnorm,
    args = list(
      mean = mean(datLong$solihullUse, na.rm = TRUE),
      sd = sd(datLong$solihullUse, na.rm = TRUE)
    ),
    colour = "black",
    size = 1
  )
grid.arrange(SKCQLongPlot1,
             SKCQLongPlot2,
             SKCQLongPlot3,
             ncol = 3)
# Cannot assume normality for SKCQ variables when collapsed across timepoints

# Check whether SKCQ variables are normally distributed at each timepoint
round(stat.desc(SKCQAll, basic = FALSE, norm = TRUE), 3)
# Cannot assume normality for most variables, check histograms
# Histograms for SKCQ variables
SKCQBW <- 0.2
SKCQPlot1 <-
  ggplot(dat, aes(SKCQ1Conf)) + geom_histogram(binwidth = SKCQBW) # Assume normal distribution
SKCQPlot2 <-
  ggplot(dat, aes(SKCQ2Conf)) + geom_histogram(binwidth = SKCQBW) # Assume normal distribution
SKCQPlot3 <-
  ggplot(dat, aes(SKCQ3Conf)) + geom_histogram(binwidth = SKCQBW) # Assume normal distribution
SKCQPlot4 <-
  ggplot(dat, aes(SKCQ4Conf)) + geom_histogram(binwidth = SKCQBW) # Cannot assume normality
SKCQPlot5 <-
  ggplot(dat, aes(SKCQ1Und)) + geom_histogram(binwidth = SKCQBW) # Assume normal distribution
SKCQPlot6 <-
  ggplot(dat, aes(SKCQ2Und)) + geom_histogram(binwidth = SKCQBW) # Assume normal distribution
SKCQPlot7 <-
  ggplot(dat, aes(SKCQ3Und)) + geom_histogram(binwidth = SKCQBW) # Assume normal distribution
SKCQPlot8 <-
  ggplot(dat, aes(SKCQ4Und)) + geom_histogram(binwidth = SKCQBW) # Assume normal distribution
SKCQPlot9 <-
  ggplot(dat, aes(SKCQ1Use)) + geom_histogram(binwidth = SKCQBW) # Cannot assume normality
SKCQPlot10 <-
  ggplot(dat, aes(SKCQ2Use)) + geom_histogram(binwidth = SKCQBW) # Assume normal distribution
SKCQPlot11 <-
  ggplot(dat, aes(SKCQ3Use)) + geom_histogram(binwidth = SKCQBW) # Assume normal distribution
SKCQPlot12 <-
  ggplot(dat, aes(SKCQ4Use)) + geom_histogram(binwidth = SKCQBW) # Assume normal distribution
grid.arrange(
  SKCQPlot1,
  SKCQPlot2,
  SKCQPlot3,
  SKCQPlot4,
  SKCQPlot5,
  SKCQPlot6,
  SKCQPlot7,
  SKCQPlot8,
  SKCQPlot9,
  SKCQPlot10,
  SKCQPlot11,
  SKCQPlot12,
  ncol = 4
)
# Statistical check for SKCQ variables appearing to deviate from normality
shapiro.test(dat$SKCQ4Conf)
# Not normally distributed, try transformations

# Tranformations for SKCQ variables violating assumption of normality
SKCQ4ConfSqrt <-
  sqrt(SKCQ4Conf) # Try square root transformation for SKCQ4Conf
shapiro.test(SKCQ4ConfSqrt)
ggplot(dat, aes(SKCQ4ConfSqrt)) + geom_histogram() # Cannot assume normality
SKCQ4ConfLog <-
  log(SKCQ4Conf) # Try log transformation for SKCQ4Conf
shapiro.test(SKCQ4ConfLog)
ggplot(dat, aes(SKCQ4ConfLog)) + geom_histogram() # Cannot assume normality
SKCQ4ConfRecip <-
  1 / (SKCQ4Conf) # Try reciprocal transformation for SKCQ4Conf
shapiro.test(SKCQ4ConfRecip)
ggplot(dat, aes(SKCQ4ConfRecip)) + geom_histogram() # Cannot assume normality
# SKCQ4Conf not normally distributed and can't be usefully transformed
SKCQ1UseSqrt <-
  sqrt(SKCQ1Use) # Try square root transformation for SKCQ1Use
shapiro.test(SKCQ1UseSqrt)
ggplot(dat, aes(SKCQ1UseSqrt)) + geom_histogram() # Cannot assume normality
SKCQ1UseLog <- log(SKCQ1Use) # Try log transformation for SKCQ1Use
shapiro.test(SKCQ1UseLog)
ggplot(dat, aes(SKCQ1UseLog)) + geom_histogram() # Cannot assume normality
SKCQ1UseRecip <-
  1 / (SKCQ1Use) # Try reciprocal transformation for SKCQ1Use
shapiro.test(SKCQ1UseRecip)
ggplot(dat, aes(SKCQ1UseRecip)) + geom_histogram() # Cannot assume normality
# SKCQ1Use not normally distributed and can't be usefully transformed
# Use non-parametric methods with SKCQ data

# Check internal consistency for Solihull Knowlege and Confidence Questionnaire
SKCQ1ConfCols <-
  cbind(dat$SKCQ1_1, dat$SKCQ1_2, dat$SKCQ1_3, dat$SKCQ1_4) # Combine columns for each Confidence subscale administration
SKCQ2ConfCols <-
  cbind(dat$SKCQ2_1, dat$SKCQ2_2, dat$SKCQ2_3, dat$SKCQ2_4)
SKCQ3ConfCols <-
  cbind(dat$SKCQ2_1, dat$SKCQ3_2, dat$SKCQ3_3, dat$SKCQ3_4)
SKCQ4ConfCols <-
  cbind(dat$SKCQ4_1, dat$SKCQ4_2, dat$SKCQ4_3, dat$SKCQ4_4)
SKCQ1UndCols <-
  cbind(dat$SKCQ1_5,
        dat$SKCQ1_6,
        dat$SKCQ1_7,
        dat$SKCQ1_8,
        dat$SKCQ1_9) # Combine columns for each Undersatnding subscale administration
SKCQ2UndCols <-
  cbind(dat$SKCQ2_5,
        dat$SKCQ2_6,
        dat$SKCQ2_7,
        dat$SKCQ2_8,
        dat$SKCQ2_9)
SKCQ3UndCols <-
  cbind(dat$SKCQ3_5,
        dat$SKCQ3_6,
        dat$SKCQ3_7,
        dat$SKCQ3_8,
        dat$SKCQ3_9)
SKCQ4UndCols <-
  cbind(dat$SKCQ4_5,
        dat$SKCQ4_6,
        dat$SKCQ4_7,
        dat$SKCQ4_8,
        dat$SKCQ4_9)
SKCQ1UseCols <-
  cbind(dat$SKCQ1_10,
        dat$SKCQ1_11,
        dat$SKCQ1_12,
        dat$SKCQ1_13,
        dat$SKCQ1_14) # Combine columns for each Understanding subscale administration
SKCQ2UseCols <-
  cbind(dat$SKCQ2_10,
        dat$SKCQ2_11,
        dat$SKCQ2_12,
        dat$SKCQ2_13,
        dat$SKCQ2_14)
SKCQ3UseCols <-
  cbind(dat$SKCQ3_10,
        dat$SKCQ3_11,
        dat$SKCQ3_12,
        dat$SKCQ3_13,
        dat$SKCQ3_14)
SKCQ4UseCols <-
  cbind(dat$SKCQ4_10,
        dat$SKCQ4_11,
        dat$SKCQ4_12,
        dat$SKCQ4_13,
        dat$SKCQ4_14)
cronbach(SKCQ1ConfCols) # Calculate Cronbach's alpha for SKCQ Confidence items
cronbach(SKCQ2ConfCols)
cronbach(SKCQ3ConfCols)
cronbach(SKCQ4ConfCols)
cronbach(SKCQ1UndCols) # Calculate Cronbach's alpha for SKCQ Understanding items
cronbach(SKCQ2UndCols)
cronbach(SKCQ3UndCols)
cronbach(SKCQ4UndCols)
cronbach(SKCQ1UseCols) # Calculate Cronbach's alpha for SKCQ Usage items
cronbach(SKCQ2UseCols)
cronbach(SKCQ3UseCols)
cronbach(SKCQ4UseCols)
# Acceptable internal consistency for SKCQ variables EXCEPT SKCQ3Conf. Fewest participants completed this one.

# Produce summary of SKCQ data for later graphing
datLongSumm <- ddply(
  datLong,
  c("timepoint"),
  summarise,
  N = sum(!is.na(solihullConf)),
  conf = mean(solihullConf, na.rm = TRUE),
  und = mean(solihullUnd, na.rm = TRUE),
  use = mean(solihullUse, na.rm = TRUE),
  sd   = sd(solihullConf, na.rm = TRUE),
  se   = sd / sqrt(N)
)

######################
## Multivariate EDA ##
######################

# Correlation matrices for Solihull Knowledge and Confidence Questionnaire
rcorr(as.matrix(SKCQAll), type = "pearson")
# All SKCQ subscales significantly intercorrelated at baseline and follow-up

# Check whether SKCQ scales vary by school
ggplot(dat, aes(factor(school), SKCQ1Conf)) +
  geom_boxplot()
ggplot(dat, aes(factor(school), SKCQ1Und)) +
  geom_boxplot()
ggplot(dat, aes(factor(school), SKCQ1Use)) +
  geom_boxplot()
fit <-
  aov(SKCQ1Conf ~ school, data = dat) # One Way Anova (Completely Randomized Design)
layout(matrix(c(1, 2, 3, 4), 2, 2)) # optional layout
plot(fit) # diagnostic plots
summary(fit) # display Type I ANOVA table
with(dat,
     pairwise.t.test(
       SKCQ1Conf,
       school,
       p.adj = "bonferroni",
       paired = F,
       na.rm = T
     ))
# School 1 significantly lower pre-training SKCQ Confidence score than school 2.
fit <-
  aov(SKCQ1Und ~ school, data = dat) # One Way Anova (Completely Randomized Design)
layout(matrix(c(1, 2, 3, 4), 2, 2)) # optional layout
plot(fit) # diagnostic plots
summary(fit) # display Type I ANOVA table
with(dat,
     pairwise.t.test(
       SKCQ1Und,
       school,
       p.adj = "bonferroni",
       paired = F,
       na.rm = T
     ))
# No significant effect of school on baseline SCKQUnd
fit <-
  aov(SKCQ1Use ~ school, data = dat) # One Way Anova (Completely Randomized Design)
layout(matrix(c(1, 2, 3, 4), 2, 2)) # optional layout
plot(fit) # diagnostic plots
summary(fit) # display Type I ANOVA table
with(dat,
     pairwise.t.test(
       SKCQ1Use,
       school,
       p.adj = "bonferroni",
       paired = F,
       na.rm = T
     ))
# No significant effect of school on baseline SCKQUse

# Check visually whether PROQOL subscales vary by school
ggplot(dat, aes(factor(dat$school), proQolPreCST)) +
  geom_boxplot()
ggplot(dat, aes(factor(dat$school), proQolPreBOT)) +
  geom_boxplot()
ggplot(dat, aes(factor(dat$school), proQolPreSTST)) +
  geom_boxplot()
# Check statistically whether PROQOL subscales vary by school
fit <-
  aov(proQolPreCS ~ school, data = dat) # One Way Anova (Completely Randomized Design)
layout(matrix(c(1, 2, 3, 4), 2, 2)) # optional layout
plot(fit) # diagnostic plots
summary(fit) # Significant effect of school on baseline PROQOL Compassion scale
with(dat,
     pairwise.t.test(
       proQolPreCS,
       school,
       p.adj = "bonferroni",
       paired = F,
       na.rm = T
     ))
fit <-
  aov(proQolPreBO ~ school, data = dat) # One Way Anova (Completely Randomized Design)
layout(matrix(c(1, 2, 3, 4), 2, 2)) # optional layout
plot(fit) # diagnostic plots
summary(fit) # Significant effect of school on baseline PROQOL Burnout scale
with(dat,
     pairwise.t.test(
       proQolPreBO,
       school,
       p.adj = "bonferroni",
       paired = F,
       na.rm = T
     ))
fit <-
  aov(proQolPreSTS ~ school, data = dat) # One Way Anova (Completely Randomized Design)
layout(matrix(c(1, 2, 3, 4), 2, 2)) # optional layout
plot(fit) # diagnostic plots
summary(fit) # No effect of school on baseline PROQOL Secondary Traumatic Stress scale
with(dat,
     pairwise.t.test(
       proQolPreSTS,
       school,
       p.adj = "bonferroni",
       paired = F,
       na.rm = T
     ))
# Significant effects of school on baseline variables
# Include school as control variable in later analysis

datSchOnly <- dat[dat$school %in% c(2:5), ] # Works to select only school-based training sites


# Check visually whether SKCQ scales vary by job
ggplot(dat, aes(factor(dat$jobCat), SKCQ4Conf)) +
  geom_boxplot()
ggplot(dat, aes(factor(dat$jobCat), SKCQ4Und)) +
  geom_boxplot()
ggplot(dat, aes(factor(dat$jobCat), SKCQ4Use)) +
  geom_boxplot()
# Check visually whether SKCQ scales vary by job
fit <-
  aov(SKCQ1Conf ~ dat$jobCat, data = dat) # One Way Anova (Completely Randomized Design)
layout(matrix(c(1, 2, 3, 4), 2, 2)) # optional layout
plot(fit) # diagnostic plots
summary(fit) # display Type I ANOVA table
with(dat,
     pairwise.t.test(
       SKCQ1Conf,
       dat$jobCat,
       p.adj = "bonferroni",
       paired = F,
       na.rm = T
     ))
# SLT's baseline SKCQ Confidence scores were significantly lower than teachers, TAs and 'others' but not FSWs
# Include job as a control variable in later analysis