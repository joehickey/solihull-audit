# Set working directory
setwd("~/R") # Amend as needed
library(ggplot2)
library(gplots)
library(car)
library(tidyr)
library(plyr)
library(nlme)
library(multcomp)
library(dplyr)
library(psy)

# ToDo:
# Make PROQOL variables right in 'long' format datset
# Sort out variables for training feedback questionnaire

######################
## DATA PREPARATION ##
######################

# Define the variable types
varTypes <-
  c(
    "character",
    "factor",
    "factor",
    "integer",
    "factor",
    "character",
    "character",
    "factor",
    "integer",
    "integer",
    rep("integer", (60 + 14 + 14 + 14 + 14 + 23)),
    rep("character", 7),
    rep("integer", 23),
    rep("character", 7),
    rep("integer", 15),
    rep("character", 7)
  )

# Read the data
parentDir <- dirname(getwd())
dat <-
  read.csv(paste(parentDir, "solihull_eval_db_20160728.csv", sep = "/"),
           colClasses = varTypes)

# Recode job into four levels
jobCatVarsAll  <-
  c(
    "coach",
    "FSW",
    "NN",
    "OT",
    "OTA",
    "other",
    "physio",
    "SLT",
    "SLTA",
    "SW",
    "TA",
    "teacher"
  )
targetSet <- c("FSW", "teacher", "TA", "SLT", "other")
dat$jobCat <- ifelse(dat$jobCat %in% targetSet, dat$jobCat, "other")
jobCatVarsFour  <- c("other", "FSW", "SLT", "teacher", "TA")
dat$jobCat <- factor(dat$jobCat, levels = jobCatVarsFour)

# Recode Knowledge Skills and Confidence Questionnaire variables
SKCQ1Conf <-
  (dat$SKCQ1_1 + dat$SKCQ1_2 + dat$SKCQ1_3 + dat$SKCQ1_4) / 4
SKCQ1Und <-
  (dat$SKCQ1_5 + dat$SKCQ1_6 + dat$SKCQ1_7 + dat$SKCQ1_8 + dat$SKCQ1_9) / 5
SKCQ1Use <-
  (dat$SKCQ1_10 + dat$SKCQ1_11 + dat$SKCQ1_12 + dat$SKCQ1_13 + dat$SKCQ1_14) / 5
SKCQ2Conf <-
  (dat$SKCQ2_1 + dat$SKCQ2_2 + dat$SKCQ2_3 + dat$SKCQ2_4) / 4
SKCQ2Und <-
  (dat$SKCQ2_5 + dat$SKCQ2_6 + dat$SKCQ2_7 + dat$SKCQ2_8 + dat$SKCQ2_9) / 5
SKCQ2Use <-
  (dat$SKCQ2_10 + dat$SKCQ2_11 + dat$SKCQ2_12 + dat$SKCQ2_13 + dat$SKCQ2_14) / 5
SKCQ3Conf <-
  (dat$SKCQ3_1 + dat$SKCQ3_2 + dat$SKCQ3_3 + dat$SKCQ3_4) / 4
SKCQ3Und <-
  (dat$SKCQ3_5 + dat$SKCQ3_6 + dat$SKCQ3_7 + dat$SKCQ3_8 + dat$SKCQ3_9) / 5
SKCQ3Use <-
  (dat$SKCQ3_10 + dat$SKCQ3_11 + dat$SKCQ3_12 + dat$SKCQ3_13 + dat$SKCQ3_14) / 5
SKCQ4Conf <-
  (dat$SKCQ4_1 + dat$SKCQ4_2 + dat$SKCQ4_3 + dat$SKCQ4_4) / 4
SKCQ4Und <-
  (dat$SKCQ4_5 + dat$SKCQ4_6 + dat$SKCQ4_7 + dat$SKCQ4_8 + dat$SKCQ4_9) / 5
SKCQ4Use <-
  (dat$SKCQ4_10 + dat$SKCQ4_11 + dat$SKCQ4_12 + dat$SKCQ4_13 + dat$SKCQ4_14) / 5
# Add new SKCQ variables to main dataset
dat$SKCQ1Conf <- SKCQ1Conf
dat$SKCQ2Conf <- SKCQ2Conf
dat$SKCQ3Conf <- SKCQ3Conf
dat$SKCQ4Conf <- SKCQ4Conf
dat$SKCQ1Und <- SKCQ1Und
dat$SKCQ2Und <- SKCQ2Und
dat$SKCQ3Und <- SKCQ3Und
dat$SKCQ4Und <- SKCQ4Und
dat$SKCQ1Use <- SKCQ1Use
dat$SKCQ2Use <- SKCQ2Use
dat$SKCQ3Use <- SKCQ3Use
dat$SKCQ4Use <- SKCQ4Use
# Make summary SKCQ variables
SKCQConf <- data.frame(SKCQ1Conf, SKCQ2Conf, SKCQ3Conf, SKCQ4Conf)
SKCQUnd <- data.frame(SKCQ1Und, SKCQ2Und, SKCQ3Und, SKCQ4Und)
SKCQUse <- data.frame(SKCQ1Use, SKCQ2Use, SKCQ3Use, SKCQ4Use)
SKCQAll <-
  data.frame(
    SKCQ1Conf,
    SKCQ2Conf,
    SKCQ3Conf,
    SKCQ4Conf,
    SKCQ1Und,
    SKCQ2Und,
    SKCQ3Und,
    SKCQ4Und,
    SKCQ1Use,
    SKCQ2Use,
    SKCQ3Use,
    SKCQ4Use
  )

# Recode Professional Quality of Life Scale
PROQOLPre_1r <-
  recode(
    dat$PROQOLPre_1,
    '1' = 5,
    '2' = 4,
    '3' = 3,
    '4' = 2,
    '5' = 1
  )
PROQOLPre_4r <-
  recode(
    dat$PROQOLPre_4,
    '1' = 5,
    '2' = 4,
    '3' = 3,
    '4' = 2,
    '5' = 1
  )
PROQOLPre_15r <-
  recode(
    dat$PROQOLPre_15,
    '1' = 5,
    '2' = 4,
    '3' = 3,
    '4' = 2,
    '5' = 1
  )
PROQOLPre_17r <-
  recode(
    dat$PROQOLPre_17,
    '1' = 5,
    '2' = 4,
    '3' = 3,
    '4' = 2,
    '5' = 1
  )
PROQOLPre_29r <-
  recode(
    dat$PROQOLPre_29,
    '1' = 5,
    '2' = 4,
    '3' = 3,
    '4' = 2,
    '5' = 1
  )
proQolPreCS <-
  dat$PROQOLPre_3 + dat$PROQOLPre_6 + dat$PROQOLPre_12 + dat$PROQOLPre_16 + dat$PROQOLPre_18 + dat$PROQOLPre_20 + dat$PROQOLPre_22 + dat$PROQOLPre_24 + dat$PROQOLPre_27 + dat$PROQOLPre_30
proQolPreBO <-
  PROQOLPre_1r + PROQOLPre_4r + dat$PROQOLPre_8 + dat$PROQOLPre_10 + PROQOLPre_15r + PROQOLPre_17r + dat$PROQOLPre_19 + dat$PROQOLPre_21 + dat$PROQOLPre_26 + PROQOLPre_29r
proQolPreSTS <-
  dat$PROQOLPre_2 + dat$PROQOLPre_5 + dat$PROQOLPre_7 + dat$PROQOLPre_9 + dat$PROQOLPre_11 + dat$PROQOLPre_13 + dat$PROQOLPre_14 + dat$PROQOLPre_23 + dat$PROQOLPre_25 + dat$PROQOLPre_28
# Add new PROQOL pre-training variables to main dataset
dat$proQolPreCS <- proQolPreCS
dat$proQolPreBO <- proQolPreBO
dat$proQolPreSTS <- proQolPreSTS

PROQOLPost_1r <-
  recode(
    dat$PROQOLPost_1,
    '1' = 5,
    '2' = 4,
    '3' = 3,
    '4' = 2,
    '5' = 1
  )
PROQOLPost_4r <-
  recode(
    dat$PROQOLPost_4,
    '1' = 5,
    '2' = 4,
    '3' = 3,
    '4' = 2,
    '5' = 1
  )
PROQOLPost_15r <-
  recode(
    dat$PROQOLPost_15,
    '1' = 5,
    '2' = 4,
    '3' = 3,
    '4' = 2,
    '5' = 1
  )
PROQOLPost_17r <-
  recode(
    dat$PROQOLPost_17,
    '1' = 5,
    '2' = 4,
    '3' = 3,
    '4' = 2,
    '5' = 1
  )
PROQOLPost_29r <-
  recode(
    dat$PROQOLPost_29,
    '1' = 5,
    '2' = 4,
    '3' = 3,
    '4' = 2,
    '5' = 1
  )
proQolPostCS <-
  dat$PROQOLPost_3 + dat$PROQOLPost_6 + dat$PROQOLPost_12 + dat$PROQOLPost_16 + dat$PROQOLPost_18 + dat$PROQOLPost_20 + dat$PROQOLPost_22 + dat$PROQOLPost_24 + dat$PROQOLPost_27 + dat$PROQOLPost_30
proQolPostBO <-
  PROQOLPost_1r + PROQOLPost_4r + dat$PROQOLPost_8 + dat$PROQOLPost_10 + PROQOLPost_15r + PROQOLPost_17r + dat$PROQOLPost_19 + dat$PROQOLPost_21 + dat$PROQOLPost_26 + PROQOLPost_29r
proQolPostSTS <-
  dat$PROQOLPost_2 + dat$PROQOLPost_5 + dat$PROQOLPost_7 + dat$PROQOLPost_9 + dat$PROQOLPost_11 + dat$PROQOLPost_13 + dat$PROQOLPost_14 + dat$PROQOLPost_23 + dat$PROQOLPost_25 + dat$PROQOLPost_28
# Add new PROQOL post-training variables to main dataset
dat$proQolPostCS <- proQolPostCS
dat$proQolPostBO <- proQolPostBO
dat$proQolPostSTS <- proQolPostSTS

# PROQOL make data frame of all pre and post scores for correlation analysis
proQolAll <-
  data.frame(proQolPreCS,
             proQolPreBO,
             proQolPreSTS,
             proQolPostCS,
             proQolPostBO,
             proQolPostSTS)

# Centre and scale ProQOL variables to produce Z-scores
proQolPreCSZ <- c(scale(proQolPreCS))
proQolPreBOZ <- c(scale(proQolPreBO))
proQolPreSTSZ <- c(scale(proQolPreSTS))
proQolPostCSZ <- c(scale(proQolPostCS))
proQolPostBOZ <- c(scale(proQolPostBO))
proQolPostSTSZ <- c(scale(proQolPostSTS))

# Convert ProQOL Z-scores to T-scores
proQolPreCST <- (proQolPreCSZ * 10) + 50
proQolPreBOT <- (proQolPreBOZ * 10) + 50
proQolPreSTST <- (proQolPreSTSZ * 10) + 50
proQolPostCST <- (proQolPostCSZ * 10) + 50
proQolPostBOT <- (proQolPostBOZ * 10) + 50
proQolPostSTST <- (proQolPostSTSZ * 10) + 50

# Calculate change scores
dat$SKCQConfChg <- SKCQ4Conf - SKCQ1Conf
dat$SKCQUndChg <- SKCQ4Und - SKCQ1Und
dat$SKCQUseChg <- SKCQ4Use - SKCQ1Use
dat$proQolCSChg <- proQolPostCS - proQolPreCS
dat$proQolBOChg <- proQolPostBO - proQolPreBO
dat$proQolSTSChg <- proQolPostSTS - proQolPreSTS

# Make a data.frame in 'long' format to assist repeated measures analysis
datLong <- dat
datLong[, grepl(("^SKCQ[0-4][CU]"), names(dat))] %>% # Select only the 12 needed columns using regular expression to catch SKCQ1_1 through SCKQ4_14
  unite(one, contains("1")) %>%  # Unite all SKCQ columns that contain '1' with default sep = "_" into single new column named "one"
  unite(two, contains("2")) %>%
  unite(three, contains("3")) %>%
  unite(four, contains("4")) %>%
  gather(timepoint, values, one:four) %>% # Gather all columns between that named "one" and that named "four" (inclusive) into two new columns: a key column (named "timepoint") and a value column (named "values")
  separate(values, c("solihullConf", "solihullUnd", "solihullUse"), sep = "_") -> datLongSKCQ # separate the column named "values" into two three columns named "solihullConf", "solihullUnd and "solihullUse" according to the separator "_".
datLong <- cbind(datLong, datLongSKCQ)
# Make timepoint variables easier to follow
datLong$timepoint <-
  recode_factor(
    datLong$timepoint,
    one = "1",
    two = "2",
    three = "3",
    four = "4",
    .missing = "NA",
    .ordered = T
  )
# Make new columns numeric
datLong$solihullConf <- as.numeric(datLong$solihullConf)
datLong$solihullUnd <- as.numeric(datLong$solihullUnd)
datLong$solihullUse <- as.numeric(datLong$solihullUse)

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
