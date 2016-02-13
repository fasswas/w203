# Exercies for DSUR Ch. 6
rm(list=ls())
setwd("~/Google Drive/UC Berkeley/Courses/W203- Exploring and Analyzing Data/R Learning")

adverts <- c(5,4,4,6,8)
packets <- c(8,9,10,13,15)
advertData <- data.frame(adverts, packets)

library(ggplot2)
plot = ggplot(advertData, aes(x=adverts, y=packets))
plot + geom_point() + xlim(0,max(adverts)) + ylim(0,max(packets))

library(Hmisc)
library(polycor)

examData <- read.delim("Exam Anxiety.dat")
summary(examData)

# Compute pearson's correlation between different numeric variables using cor()
examData2 <- examData[, c("Exam", "Anxiety", "Revise")]
cor(examData2) # Find correlations between every pair of variables.
cor = cor(examData[, c("Exam", "Revise")]) # Find correlations between Exam and Anxiety.
cor
cor^2 # cor() returns a matrix.  Square it to get the "coefficient of determinant"

examMatrix <- as.matrix(examData[,c("Exam", "Anxiety", "Revise")])
library(Hmisc)
rcorr(examMatrix)

# Perform Pearson's correlation test
cor.test(x=examData$Exam, y=examData$Anxiety, method = "pearson")
cor.test(x=examData$Exam, y=examData$Anxiety, alternative="less", method = "pearson")
cor.test(x=examData$Exam, y=examData$Anxiety, alternative="greater", method = "pearson")


liarData = read.delim("The Biggest Liar.dat")
cor(liarData$Position, liarData$Creativity, method = "spearman")

cor.test(x=liarData$Position, y=liarData$Creativity, alternative = "less", method="spearman")

# r^2: coefficient of determination
cor(examData2) ^ 2

# partial correlation
library(ggm)
pc <- pcor(c("Exam", "Anxiety", "Revise"), var(examData2)) # Control for Revise
pcor.test(pc, 1, 103)

# chi-squared tests
summary(diamonds)
table(diamonds$color, diamonds$clarity)

# Is there relationship between color and clarity?
cs = chisq.test(diamonds$color, diamonds$clarity)
cs # The p-value is 2.2e-16, and so we'll reject the null hypothesis that "they are independent".
cs$expected # See what we expect if those two are independent.  Compare it to table()

# Look at the std. residuals to see which color contribute most to the result。
# Standardized residuals can be interpreted as z scores (as is the case with other z scores, 
# a value outside of roughly +/− 1.96 will be statistically significant.)
cs$stdres

# Chi-square statistic just tells you how likely the two variables are independent.
# If they're dependent, Crammer's V tells you the strength of the relationship.
# Values range from 0 to 1
# Under 0.2 = weak
# Between 0.2 and 0.4 = medium/strong
# Over 0.4 = very strong
#
# Crammer's V
crammers_v = function(cs)
{
  cv = sqrt(cs$statistic / (sum(cs$observed) * (min( dim(cs$observed) ) - 1 )))
  return(as.numeric(cv))
}
