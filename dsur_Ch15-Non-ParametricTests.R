# Exercies for DSUR Ch. 15
rm(list=ls())
setwd("~/Google Drive/UC Berkeley/Courses/W203- Exploring and Analyzing Data/R Learning")

drugData <- read.delim("Drug.dat")

library(ggplot2)
library(clinfun)
library(pgirmess)
library(library(pastecs))

# Use Shapiro-Wilk's test to test for normality

# Run the test, grouped by drug
by(drugData[,2:3], drugData$drug, stat.desc, basic=FALSE, norm=TRUE)

# Use Levene's Test for testing homogeneity of variance
library(car)
leveneTest(drugData$sundayBDI, drugData$drug, center="mean")
leveneTest(drugData$wedsBDI, drugData$drug, center="mean")

# Run the Wilcoxon Rank-Sum test
# For testing the differences in two independent samples
sunModel = wilcox.test(sundayBDI ~ drug, data = drugData, paired=FALSE)
wedModel = wilcox.test(wedsBDI ~ drug, data = drugData, paired=FALSE)

# Results:
# Sunday: the type of drug did not significantly affect depression levels the day after, W = 35.5, p = .286.
# Wednesday: the type of drug did significantly affect depression levels the day after, W = 4, p < .001.

# Effect size: r = z / sqrt(N)
rFromWilcox <- function(wilcoxModel, N)
{
  z <- qnorm(wilcoxModel$p.value/2)
  r <- z / sqrt(N)
  cat(wilcoxModel$data.name, "Effect size, r = ", r)
}

rFromWilcox(sunModel, nrow(drugData))
rFromWilcox(wedModel, nrow(drugData))


#########################################
# Perform Wilcoxon Signed-Rank Test
# It is used in situations in which there are two sets of scores to compare, but these 
# scores come from the same participants.

# First check the normality of the "difference between the scores of the two different drugs",
# taken by the same participant.

# Compute the change in BDI scores from Sunday to Wednesday and then compute normality tests
# for this change score separately for the alcohol and ecstasy groups.
drugData$diff = drugData$wedsBDI - drugData$sundayBDI
by(drugData$diff, drugData$drug, stat.desc, basic=FALSE, norm=TRUE)

# For the alcohol group we have a non-normal distribution, W = 0.83, p < .05, in the change scores. 
# Therefore, we need to use a non-parametric test for the alcohol group

# Use the subset() function to create separate dataframes for the different drugs, called 
# alcoholData and ecstasyData.
alcoholData <- subset(drugData, subset = drugData$drug == "Alcohol")
ecstasyData <- subset(drugData, subset = drugData$drug == "Ecstasy")

# Call the function using wide format.  We want to turn off auto correction.
alcoholModel = wilcox.test(alcoholData$wedsBDI, alcoholData$sundayBDI, paired=TRUE, correct=FALSE)
alcoholModel
# It reports the value of T + (which it calls V) and that this value is significant at p = .047 . 
# Therefore , we should conclude (based on the medians) that when taking alcohol there was a 
# significant decline in depression (as measured by the BDI) from the morning after 
# to midweek (p = .047).

# Effect size
rFromWilcox(alcoholModel, 20)
