# Exercies for DSUR Ch. 10
rm(list=ls())
setwd("~/Google Drive/UC Berkeley/Courses/W203- Exploring and Analyzing Data/R Learning")

# Note:
# Complete script from the book:
# http://studysites.sagepub.com/dsur/study/DSUR%20R%20Script%20Files/Chapter%2010%20DSUR%20GLM1.R

#install.packages("compute.es") # For effect size
#install.packages("multcomp") # For post hoc tests
#install.packages("pastecs") # For descriptive statistics
#install.packages("WRS2") # For robust tests

library(compute.es)
library(multcomp)
library(pastecs)
library(WRS2)
library(car)

viagraData = read.delim("Viagra.dat", header = T)
viagraData$dose = factor(x = viagraData$dose, levels=c(1,2,3), labels = c("Placebo", "Low Dose", "High Dose"))

# Graph
plot = ggplot(viagraData, aes(x=dose, y=libido))
plot + stat_summary(fun.data = mean_cl_boot, geom="errorbar", width=0.2) +
  stat_summary(fun.y = mean, geom="line", aes(group=1)) +
  stat_summary(fun.y = mean, geom = "point", size = 4, colour = "#990000")

# Describe
by(viagraData$libido, viagraData$dose, stat.desc)
# Mean libido by the 3 different dose category:
# Placebo: 2.2
# Low: 3.2
# High: 5.0

# Check variability using Levene's Test
leveneTest(viagraData$libido, viagraData$dose,center = median)

#######################
# ANOVA
# We are predicting libido from group membership (i.e., the categorical variable dose) 
# so our model is: libido_i = dose_i + error_i (where dose is a categorical parameter)
viagraModel = aov(libido ~ dose, data = viagraData)
summary(viagraModel) # This command gives a summary of the overall ANOVA 

###
# Interpret the result

# Based on our model: 
# SS_M = 20.13, SS_R = 23.60, MS_M = 10.067, MS_R = 1.967

# F-Ratio for dose = MS_M / MS_R = 5.119, Pr(>F) = 0.0247
# The F-Ratio is larger than 1, which indicates that the experimental manipulation had some effect 
# above and beyond the effect of individual differences in performance.
# The p-value of 0.0247 tells us that this result is statistically significant, that it's unlikely
# we get this F-value just by chance.

plot(viagraModel)

# Use Welch Test if leveneTest is significant, meaning our population variances are not similar
# across our groups.
# Welch Test
oneway.test(libido~dose, data = viagraData)

#######
# Post-Test analysis

# This gives a summary of the parameters of the linear model (rather than the overall ANOVA).
# libido_i = b0 + b1*low_i + b2*high_i + e_i
# where low_i and high_i are dummy variables taking value of either 0 or 1
summary.lm(viagraModel) # Summarize Linear Model Fits

###
# Interpret the result
# From the "Estimate" column, our linear model is:
# libido_i = 2.2 + 1*low_i + 2.8*high_i
#
# Note: the mean value of each category can be derived from this formula

# There is a t-statistic and a Pr(>|t|) associated with each model parameter.
# So, the ‘low dose’ effect compared to placebo is non-significant (t = 1.13, p = .282), 
# whereas the effect of high dose compared to the placebo group is significant (t = 3.16, p = .008).

#######
# Post hoc tests

pairwise.t.test(viagraData$libido, viagraData$dose, p.adjust.method = "bonferroni")
###
# Interpret the result
# let’s look at the Bonferroni corrected values: 
# the placebo group is compared to the low-dose group and reveals a non-significant difference 
# (. 845 is greater than .05), 
# But when compared to the high-dose group there is a significant difference  (. 025 is less than .05).
# The low-dose group compared to the high-dose group is not significant (because 0.196 is greater than .05).

pairwise.t.test(viagraData$libido, viagraData$dose, p.adjust.method = "BH")

# Tukey and Dunnett can be implemented using the glht() function that is part of the multcomp package
postHocs<-glht(viagraModel, linfct = mcp(dose = "Tukey"))
summary(postHocs)
confint(postHocs)

postHocs<-glht(viagraModel, linfct = mcp(dose = "Dunnett"), base = 1)
summary(postHocs)
confint(postHocs)

lincon(viagraWide, tr = .1)
mcppb20(viagraWide, tr = .2, nboot = 2000)

lincon(viagraWide)
mcppb20(viagraWide)

#--------Effect Sizes----------

mes(2.2, 3.2, 1.3038405, 1.3038405, 5, 5)
mes(2.2, 5, 1.3038405, 1.5811388, 5, 5)
mes(3.2, 5, 1.3038405, 1.5811388, 5, 5)

rcontrast<-function(t, df)
{r<-sqrt(t^2/(t^2 + df))
 print(paste("r = ", r))
}

rcontrast(2.474, 12)
rcontrast(2.029, 12)

omega<-function(SSm, SSr, dfm, MSr)
{
  SSt = SSm + SSr
  omega = (SSm-(dfm*MSr))/(SSt+MSr)
  print(paste("Omega-Squared: ", omega))
}

omega(20.133, 23.600, 2, 1.9667) # see summary(viagraModel)
# People normally report ω2, and it has been suggested that values of .
# 01, .06 and .14 represent small, medium and large effects respectively (Kirk, 1996). 

omega(450.66,38.09, 5, 0.334)
omega(4180.6, 4356.6, 3, 167.56)