# Exercies for DSUR Ch. 9
rm(list=ls())
setwd("~/Google Drive/UC Berkeley/Courses/W203- Exploring and Analyzing Data/R Learning")

spiderLong <- read.delim("SpiderLong.dat")
spiderWide <- read.delim("SpiderWide.dat")

spiderLong$Group = factor(spiderLong$Group)

library(ggplot2)

####### Independent t Test #######

# View the data visually.

# Error bar
plot = ggplot(spiderLong, aes(x=Group, y=Anxiety))
plot + stat_summary(fun.y = mean, geom = "bar", fill="white", color="black") +
  stat_summary(fun.data = mean_cl_normal, geom="errorbar", width=0.2) +
  labs(x="Type of Stimulus", y="Anxiety") +
  ggtitle("Independent Design\n") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))

# Box plot
plot = ggplot(spiderLong, aes(x=Group, y=Anxiety))
plot + geom_boxplot() +
  labs(x="Type of Stimulus", y="Anxiety") +
  ylim(0,100) +
  ggtitle("Boxplot of independent test\n")

# Use stat.desc() mainly to check the assumption about the normality of the data
# Both normality tests are non-significant (p = .852 for the picture group and p = .621 for 
# the real group) implying that we can probably assume normality of errors in the model.
# Note: normtest.W = the statistic of a Shapiro-Wilk test of normality
library(pastecs)
by( spiderLong$Anxiety, spiderLong$Group, stat.desc, basic = FALSE, norm = TRUE)


# paired determines whether or not you want to do a paired/ dependent t-test 
# (in which case include paired = TRUE) or an independent t-test 
# (in which case exclude the option because this is the default, or include paired = FALSE).

# For long data format:
ind.t.test = t.test(Anxiety ~ Group, data = spiderLong, paired = FALSE)
# In the result
# - The confidence intervals give the range of the difference that we would expect to include the 
#   true difference on 95% of occasions.

# Another robust test from the WRS package
# first: install dependent packages
#install.packages(c("MASS", "akima", "robustbase"))
# second: install suggested packages
#install.packages(c("cobs", "robust", "mgcv", "scatterplot3d", "quantreg", "rrcov", "lars", "pwr", "trimcluster", "parallel", "mc2d", "psych", "Rfit"))
# third: install WRS
#install.packages("WRS", repos="http://R-Forge.R-project.org", type="source")
library(WRS)
yuen(spiderWide$real, spiderWide$picture)

# Get the effect size
t <- ind.t.test$statistic[[1]]
df <- ind.t.test$parameter[[1]]
r <- sqrt( t ^ 2/( t ^ 2 + df))
round(r,3)

##################################################

library(reshape2)
spiderRepeated = melt(spiderWide)
names(spiderRepeated) = c("Group", "Anxiety") 
spiderRepeated$Group = ifelse(spiderRepeated$Group == "picture", "Picture", "Real Spider")

plot = ggplot(spiderRepeated, aes(x=Group, y=Anxiety))
plot + stat_summary(fun.y = mean, geom = "bar", fill="white", color="black") +
  stat_summary(fun.data = mean_cl_normal, geom="errorbar", width=0.2) +
  ylim(0, 60) +
  labs(x="Type of Stimulus", y="Anxiety") +
  ggtitle("Repeated Measures Design\n") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))

# Correct the repeated measures error bars
spiderWide$pMean <- (spiderWide$picture + spiderWide$real)/2
grandMean <- mean(c(spiderWide$picture, spiderWide$real))
spiderWide$adj <- grandMean - spiderWide$pMean
spiderWide$picture_adj <- spiderWide$picture + spiderWide$adj
spiderWide$real_adj <- spiderWide$real + spiderWide$adj
spiderWide$pMean2 <- (spiderWide$picture_adj + spiderWide$real_adj)/2

spiderRepeated_adj = melt(spiderWide, id.vars=NULL, measure.vars=c("picture_adj", "real_adj") )
names(spiderRepeated_adj) = c("Group", "Anxiety") 

plot = ggplot(spiderRepeated_adj, aes(x=Group, y=Anxiety))
plot + stat_summary(fun.y = mean, geom = "bar", fill="white", color="black") +
  stat_summary(fun.data = mean_cl_normal, geom="errorbar", width=0.2) +
  ylim(0, 60) +
  labs(x="Type of Stimulus", y="Anxiety") +
  ggtitle("Repeated Measures Design (Adjusted)\n") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))

####### Dependent t Test #######

# For dependent t-Test, check if the "differenece distribution" is normal
spiderWide$diff = spiderWide$picture - spiderWide$real
stat.desc(spiderWide$diff, basic = F, norm = T)

# Do a dependent t-Test, this time based on wide format data
dep.t.test = t.test(spiderWide$real, spiderWide$picture, paired = TRUE)
dep.t.test

# Another robust test from the WRS package
yuend(spiderWide$real, spiderWide$picture)

# Get the effect size
t <- dep.t.test$statistic[[1]]
df <- dep.t.test$parameter[[1]]
r <- sqrt( t ^ 2/( t ^ 2 + df))
round(r,3)