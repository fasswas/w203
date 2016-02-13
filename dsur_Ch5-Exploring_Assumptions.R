# Exercies for DSUR Ch. 5
rm(list=ls())
setwd("~/Google Drive/UC Berkeley/Courses/W203- Exploring and Analyzing Data/R Learning")

library(car)
library(ggplot2)
library(pastecs)
library(psych)
library(Rcmdr)

dlf = read.delim("DownloadFestival(No Outlier).dat", header = T)
summary(dlf)

###################################################################
# Inspect the Normal Distribution assumption using a ggplot2

# Get a density plot
hist.day1 = ggplot(dlf, aes(day1))
hist.day1 + geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(x = "Hygiene score on day 1", y = "Density") +
  theme(legend.position = "none") +
  # Add a normal distribution layer on top of our density plot
  stat_function(fun = dnorm, 
                  args = list(mean = mean(dlf$day1, na.rm = T),
                            sd = sd(dlf$day1, na.rm = T)),
                  color = "black", size = 1)

# Inspect the Normal Distribution assumption using a Q-Q plot
qqplot.day1 = qplot(sample = dlf$day1, stat="qq")
qqplot.day1


############################
# Data summmary

# From the psych library
describe(dlf$day1)

# From the pastecs library()
stat.desc(dlf$day1, basic=F, norm = T)

# describe multiple variables at the same time, so that we can compare them side by side
describe(cbind(dlf$day1, dlf$day2, dlf$day3))
stat.desc(cbind(dlf$day1, dlf$day2, dlf$day3), basic=F, norm = T)

# To avoid seeing too many decimals, use the round function :-)
round(stat.desc(cbind(dlf$day1, dlf$day2, dlf$day3), basic=F, norm = T), digits = 3)

###################################################################

rexam = read.delim("rexam.dat", header=T)
options(scipen=999, digits=10)
stat.desc(rexam, basic=F, norm=T)

head(rexam$uni)
# Let's convert rexam$uni into a factor
rexam$uni = factor(rexam$uni, levels = c(0:1), 
                   labels = c("King's College", "Queen's University"))

hist.exam = ggplot(rexam, aes(exam))
hist.exam + geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(x = "First Year Exam Score", y = "Density") +
  theme(legend.position = "none") +
  # Add a normal distribution layer on top of our density plot
  stat_function(fun = dnorm, 
                args = list(mean = mean(rexam$exam, na.rm = T),
                            sd = sd(rexam$exam, na.rm = T)),
                color = "blue", size = 1)

# Get description of the data, grouped by univeristy
by(data = rexam$exam, INDICES = rexam$uni, FUN = describe)

# Plot the exam, by university

kings.data = subset(rexam, uni == "King's College")
queens.data = subset(rexam, uni == "Queen's University")

kings.hist.exam = ggplot(kings.data, aes(exam))
kings.hist.exam + geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(x = "First Year Exam Score", y = "Density") +
  theme(legend.position = "none") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(kings.data$exam, na.rm = T),
                            sd = sd(kings.data$exam, na.rm = T)), color = "blue", size = 1)

queens.hist.exam = ggplot(queens.data, aes(exam))
queens.hist.exam + geom_histogram(aes(y=..density..), colour="black", fill="white") +
  labs(x = "First Year Exam Score", y = "Density") +
  theme(legend.position = "none") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(queens.data$exam, na.rm = T),
                            sd = sd(queens.data$exam, na.rm = T)), color = "blue", size = 1)

# Shapiro-Wilk test
shapiro.test(rexam$exam)
shapiro.test(queens.data$exam)

# Test for homogeneity of variance by using the Levene's Test.
# If the test is insignificant, it means the variance in different group (e.g. based on uni)
# are similar.
leveneTest(rexam$exam, rexam$uni)
