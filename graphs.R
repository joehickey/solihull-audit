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


# Plot change in SCKQ variables
pd <- position_dodge(width = 0.5)
ggplot(datLongSumm, aes(x = timepoint, group = 1)) +
  geom_line(aes(y = conf, colour = "conf", group = 1), position = pd) +
  geom_line(aes(y = und, colour = "und", group = 1), position = pd) +
  geom_line(aes(y = use, colour = "use", group = 1), position = pd) +
  geom_point(aes(y = conf, colour = "conf", group = 1),
             position = pd,
             size = 2) +
  geom_point(aes(y = und, colour = "und", group = 1),
             position = pd,
             size = 2) +
  geom_point(aes(y = use, colour = "use", group = 1),
             position = pd,
             size = 2) +
  geom_errorbar(
    aes(
      ymin = conf - se,
      ymax = conf + se,
      colour = "conf",
      group = 1
    ),
    width = .1,
    position = pd
  ) +
  geom_errorbar(
    aes(
      ymin = und - se,
      ymax = und + se,
      colour = "und",
      group = 1
    ),
    width = .1,
    position = pd
  ) +
  geom_errorbar(
    aes(
      ymin = use - se,
      ymax = use + se,
      colour = "use",
      group = 1
    ),
    width = .1,
    position = pd
  ) +
  ylab("Rating") +
  xlab("Time") +
  scale_x_discrete(
    breaks = c("1", "2", "3", "4"),
    labels = c(
      "Before",
      "After\nwhole day",
      "After\ntwilight",
      "After\n6 months"
    )
  ) +
  ggtitle("Solihull Training Effects") +
  scale_colour_discrete(name  = "Subscale",
                        labels = c("Confidence", "Understanding", "Usage")) +
  theme(legend.justification = "right",
        legend.position = c(1, 0.17))
