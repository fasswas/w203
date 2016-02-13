# Exercies for DSUR Ch. 7
rm(list=ls())
setwd("~/Google Drive/UC Berkeley/Courses/W203- Exploring and Analyzing Data/R Learning")

library(QuantPsyc)
library(boot)
library(car)

album1 = read.delim("Album Sales 1.dat", header = T)
albumSales.1 = lm(sales ~ adverts, data = album1)
summary(albumSales.1)

# Multiple R-squared:  0.3346,  Adjusted R-squared:  0.3313

# Let's calcuate R from R-squared
sqrt(0.3346) # 0.578
# So the Pearson's r is 0.578.


######################
# Interpretation

#####
# R-Squared:
# The value of R-squared of .335 also tells us that our model (baesd on advertisting) can account for 
# (or can explain) 33.5% of the variation in album sales.

# This means that 67% of the variation in album sales cannot be explained by advertising alone. 
# Therefore, there must be other variables that have an influence also.

# The adjusted R-squared of .331 means that our model explains 33.1% of the variation in album sales
# in the population.

# The value of Pearson's r of .578 means there is a strong positive correlation between advertising 
# and album sales.

#####
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 1.341e+02  7.537e+00  17.799   <2e-16 ***
# adverts     9.612e-02  9.632e-03   9.979   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Back to our equation Y = b0 + X.b1 + e
# b0 = 1.341e+02 = 134.1
# b1 = 9.612e-02 = 0.096 # represents the change in the outcome associated with a unit change in the predictor.
#
#  9.979   <2e-16 ***
# The t-value of the corresponding probability (based on T distribution) for each the slope estimate.
# The t-test is signficiant, meaning we can reject the null hypothesis that "b1 is zero".

# We can conclude that the advertising budget makes a statistically significant contribution (p < .001) 
# to predicting album sales.  (i.e. the contribution we see in this sample doesn't come just by chance)

#####
# F-statistic: 99.59 on 1 and 198 DF, p-value: < 2.2e-16

# F-ratio is larger than 1, which indicates that the experimental manipulation had some effect 
# above and beyond the effect of individual differences in performance.
#
# But we still need to check if we get this F value just by chance if the null hypothesis were true.
# F is significant at p < 0.001.  This tells us that there is less than a 0.1% chance that an F-ratio 
# this large would happen if the null hypothesis were true. Therefore, we can conclude that our 
# regression model results in significantly better prediction of album sales than if we used just
# the mean value of album sales.


##############
# Regression Diagnostics

plot(albumSales.1)
# See:
# http://www.stat.berkeley.edu/~spector/s133/Lr0.html
# http://stats.stackexchange.com/questions/58141/interpreting-plot-lm 

# Outlier Test:
# As long as we don't have any residual with p < 0.05, we don't have outliers.
outlierTest(albumSales.1)

# Autocorrelation Test:
# Use Durbin-Watson test to detect the presence of autocorrelation in the errors.
# A significant result means the presence of autocorrelation
dwt(albumSales.1)

# Heteroskedasticity Test:
# Use Breusch-Pagan test.  We test a null hypothesis of homoskedasticity.
library(lmtest)
bptest(albumSales.1)
# Result: p-value = 0.007378, which means we may have a problem of heteroskedasticity
# with our data.

# Assuming we really have a problem of heteroskedasticity, let's check the
# heteroskedasticity-robust errors.
# The regression line is still the same, but we've adjusted our estimates of significance.
library(sandwich)
coeftest(albumSales.1, vcov = vcovHC)

#####################################################
#####################################################

#----access the album2 data----
album2<-read.delim("Album Sales 2.dat", header = TRUE)


#---Run the multiple regression model----
albumSales.2<-lm(sales ~ adverts, data = album2)
albumSales.3<-lm(sales ~ adverts + airplay + attract, data = album2)
# Note: We could also have used the update() function to create model 3 because this model is 
# simply adding new predictors to the previous model

summary(albumSales.2)

summary(albumSales.3)
# Interpretation
#
# When airplay and attract are included as well, R^2 increased from .335 to .665.
# So model 3 accounts for 66.5% of the variance in album sales. 
# Therefore, if advertising along accounts for 33.5%, we can tell that attractiveness and radio play 
# account for an additional 33.0%.

# The adjusted R^2 (0.660) gives us some idea of how well our model generalizes, and ideally we would like 
# its value to be the same, or very close to, the value of R^2.

#---We can obtain standardized parameter estimates with the lm.beta() function---
library(QuantPsyc)
lm.beta(albumSales.3)
# - The standardized parameter estimates tell us the number of standard deviations by which the outcome will 
#   change as a result of one standard deviation change in the predictor.
# - The standardized beta values are all measured in standard deviation units and so are directly comparable.

#---Confidence intervals of the parameter estimates are obtained with the confint() function----
confint(albumSales.3)
# Imagine that we collected 100 samples of data measuring the same variables as our current model. 
# For each sample we could create a regression model to represent the data. If the model is reliable 
# then we hope to find very similar parameters in all samples.
#
# So a good model will have a small confidence interval, indicating that the value of b in this sample is 
# close to the true value of b in the population.
#
# We would expect a very bad model to have confidence intervals that cross zero, indicating that in 
# some samples the predictor has a negative relationship to the outcome whereas in others it has a 
# positive relationship.
#
# The interval for "attract" is wider (but still does not cross zero), indicating that the parameter for 
# this variable is less representative, but nevertheless significant.


# ---To compare the R2 in two models, use the ANOVA command---
anova(albumSales.2, albumSales.3)
# It’s worth noting that we can only compare hierarchical models; that is to say, the second model must 
# contain everything that was in the first model plus something new, and the third model must contain 
# everything in the second model plus something new.


#----Obtain casewise diagnostics and add them to the original data file.---

album2$residuals<-resid(albumSales.3)
album2$standardized.residuals <- rstandard(albumSales.3)
album2$studentized.residuals <- rstudent(albumSales.3)
album2$cooks.distance<-cooks.distance(albumSales.3)
album2$dfbeta <- dfbeta(albumSales.3)
album2$dffit <- dffits(albumSales.3)
album2$leverage <- hatvalues(albumSales.3)
album2$covariance.ratios <- covratio(albumSales.3)

#Save file
write.table(album2, "Album Sales With Diagnostics.dat", sep = "\t", row.names = FALSE)
#look at the data (and round the values)
round(album2, digits = 3)


#----List of standardized residuals greater than 2--------------
album2$standardized.residuals > 2 | album2$standardized.residuals < -2
# In an ordinary sample we would expect 95% of cases to have standardized residuals within about ± 2. 
# We have a sample of 200, therefore it is reasonable to expect about 10 cases (5%) to have standardized 
# residuals outside these limits.

#---Create a variable called large.residual, which is TRUE (or 1) if the residual is greater than 2, or less than -2.----------
album2$large.residual <- album2$standardized.residuals > 2 | album2$standardized.residuals < -2

#---Count the number of large residuals-------------
sum(album2$large.residual)


#---Display the value of sales, airplay, attract, adverts, and the standardized residual, for those cases 
#   which have a residual greater than 2 or less than -2.-------------
album2[album2$large.residual,c("sales", "airplay", "attract", "adverts", "standardized.residuals")]

#-----Cook's distance, leverage and covariance ratio for cases with large residuals.---------
album2[album2$large.residual , c("cooks.distance", "leverage", "covariance.ratios")]
# The average leverage can be calculated as 0.02 (k + 1/ n = 4/ 200) and so we are looking for values either twice 
# as large as this (0.04) or three times as large (0.06) depending on which statistician you trust most 
# (see section 7.7.1.2).

#----The Durbin-Watson test is obtained with either dwt() or durbinWatsonTest()---
durbinWatsonTest(albumSales.3)
dwt(albumSales.3)
# As a conservative rule I suggested that values less than 1 or greater than 3 should definitely raise alarm bells. 
# The closer to 2 that the value is, the better.


#### Assessing the assumption of no multicollinearity
#----Obtaining the VIF---
vif(albumSales.3)

#----The tolerance is 1/VIF---
1/vif(albumSales.3)

#----The mean VIF---
mean(vif(albumSales.3))


#### Checking assumptions about the residuals
#---Histogram of studentized residuals---

hist(album2$studentized.residuals)
hist(rstudent(albumSales.3))

#--Plot of residuals against fitted (predicted) values, with a flat line at the mean--
plot(albumSales.3$fitted.values,rstandard(albumSales.3))
abline(0, 0)

#same as above
plot(albumSales.3)

###########################
#Publication quality graphs

album2$fitted <- albumSales.3$fitted.values

histogram<-ggplot(album2, aes(studentized.residuals)) + opts(legend.position = "none") + geom_histogram(aes(y=..density..), colour="black", fill="white") + labs(x = "Studentized Residual", y = "Density")
histogram + stat_function(fun = dnorm, args = list(mean = mean(album2$studentized.residuals, na.rm = TRUE), sd = sd(album2$studentized.residuals, na.rm = TRUE)), colour = "red", size = 1)
ggsave(file = paste(imageDirectory,"07 album sales ggplot Hist.png",sep="/"))

scatter <- ggplot(album2, aes(fitted, studentized.residuals))
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red")+ labs(x = "Fitted Values", y = "Studentized Residual") 
ggsave(file=paste(imageDirectory,"07 Album sales ggplot scatter.png",sep="/"))

qqplot.resid <- qplot(sample = album2$studentized.residuals, stat="qq") + labs(x = "Theoretical Values", y = "Observed Values") 
qqplot.resid
ggsave(file=paste(imageDirectory,"07 Album sales ggplot QQ.png",sep="/"))


#---R tends to give values to too many decimal places, you can usefully round these values to 2 decimals.
round(rstandard(albumSales.3), 2)


#######################################################
# Robust regression (when the assumptions are violated)
##------Bootstrapping------
#---Write a bootstrap function.
object<-boot(data,function,replications)

bootReg<-function(formula, data, i)
{
  d <- data[i,]
  fit <- lm(formula, data = d)
  return(coef(fit))
}

#----bootstrapping our regression model, with 2000 replications---
bootResults<-boot(statistic = bootReg, formula = sales ~ adverts + airplay + attract, data = album2, R = 2000,)

#---We can then obtaine the bootstrap confidence intervals for the intercept:---
boot.ci(bootResults, type = "bca", index = 1)

#---And the three slope estimates---
boot.ci(bootResults, type = "bca", index = 2)
boot.ci(bootResults, type = "bca", index = 3)
boot.ci(bootResults, type = "bca", index = 4)


#######################################################

#-----Read in data for Glastonbury Festival Regression----
gfr<-read.delim(file="GlastonburyFestivalRegression.dat", header = TRUE)

#Print the first 10 cases of the dataframe
head(gfr, n = 10)

#######
# Create dummy variables for a categorical variables (manually)
#set contrasts quickly
contrasts(gfr$music)<-contr.treatment(4, base = 4)
#set contrasts with helpful names

crusty_v_NMA<-c(1, 0, 0, 0)
indie_v_NMA<-c(0, 1, 0, 0)
metal_v_NMA<-c(0, 0, 1, 0)
contrasts(gfr$music)<-cbind(crusty_v_NMA, indie_v_NMA, metal_v_NMA)



#----Exactly the same results can be obtained with------
glastonburyModel<-lm(change ~ music, data = gfr) 
summary(glastonburyModel)


#---To produce group means of each of the four groups-----
round(tapply(gfr$change, gfr$music, mean, na.rm=TRUE), 3)


#********************Labcoat Leni*******************************

#Load data & set gender to be a factor

PersonalityData<-read.delim("Chamorro-Premuzic.dat", header = TRUE)
PersonalityData$Gender<-factor(PersonalityData$Gender, levels = c(0:1), labels = c("Female", "Male"))

#Create dataframes containing variables for each analysis (need to do this because of missing values). Drop variables not in analysis
dropVars<-names(PersonalityData) %in% c("lecturerE","lecturerO", "lecturerA", "lecturerC")
neuroticLecturer<-PersonalityData[!dropVars]

dropVars<-names(PersonalityData) %in% c("lecturerN","lecturerO", "lecturerA", "lecturerC")
extroLecturer<-PersonalityData[!dropVars]

dropVars<-names(PersonalityData) %in% c("lecturerE","lecturerN", "lecturerA", "lecturerC")
openLecturer<-PersonalityData[!dropVars]

dropVars<-names(PersonalityData) %in% c("lecturerE","lecturerO", "lecturerN", "lecturerC")
agreeLecturer<-PersonalityData[!dropVars]

dropVars<-names(PersonalityData) %in% c("lecturerE","lecturerO", "lecturerA", "lecturerN")
concLecturer<-PersonalityData[!dropVars]

#Delete cases with any missing values on any variable
neuroticLecturer <-neuroticLecturer[complete.cases(neuroticLecturer),]
extroLecturer <-extroLecturer[complete.cases(extroLecturer),]
openLecturer <-openLecturer[complete.cases(openLecturer),]
agreeLecturer <-agreeLecturer[complete.cases(agreeLecturer),]
concLecturer <-concLecturer[complete.cases(concLecturer),]

#-----Neurotic Lecturer-----------
#-----Create two models-------
LecturerN.1<- lm(lecturerN ~ Age + Gender, data= neuroticLecturer)
LecturerN.2 <- lm(lecturerN ~ Age + Gender + studentN + studentE + studentO + studentA + studentC, data= neuroticLecturer)
#-----Run an anova to compare the two models------
anova(LecturerN.1, LecturerN.2)
#-----To obtain output----
summary(LecturerN.1)
summary(LecturerN.2)
#----Statistics------
vif(LecturerN.2)
dwt(LecturerN.2)

#---Histogram-----
hist(rstudent(LecturerN.2))

#-----Confidence intervals-----
confint(LecturerN.2)

##-----obtain the standardized beta estimates:------
install.packages("QuantPsyc")
Library(QuantPsyc)
lm.beta(LecturerN.1)
lm.beta(LecturerN.2)
#-----Extroverted Lecturer-----------
#----Create two models-------
LecturerE.1 <- lm(lecturerE ~ Age + Gender, data=extroLecturer)
LecturerE.2 <- lm(lecturerE ~ Age + Gender + studentN + studentE + studentO + studentA + studentC, data= extroLecturer)
#-----Run an anova to compare the two models------
anova(LecturerE.1, LecturerE.2)
#-----To obtain output----
summary(LecturerE.1)
summary(LecturerE.2)
#----Statistics------
vif(LecturerE.2)
dwt(LecturerE.2)

#---Histogram-----
hist(rstudent(LecturerE.2))

#-----Confidence intervals-----
confint(LecturerE.2)

##-----obtain the standardized beta estimates:------
install.packages("QuantPsyc")
Library(QuantPsyc)
lm.beta(LecturerE.1)
lm.beta(LecturerE.2)
#-----Openness to Experience Lecturer-----------
#----Create two models-------
LecturerO.1 <- lm(lecturerO ~ Age + Gender, data=openLecturer)
LecturerO.2 <- lm(lecturerO ~ Age + Gender + studentN + studentE + studentO + studentA + studentC, data=openLecturer)
#-----Run an anova to compare the two models------
anova(LecturerO.1, LecturerO.2)
#-----To obtain output----
summary(LecturerO.1)
summary(LecturerO.2)
#----Statistics------
vif(LecturerO.2)
dwt(LecturerO.2)

#---Histogram-----
hist(rstudent(LecturerO.2))

#-----Confidence intervals-----
confint(LecturerO.2)

##-----obtain the standardized beta estimates:------

lm.beta(LecturerO.1)
lm.beta(LecturerO.2)
#-----Agreeableness Lecturer-----------
#----Create two models-------
LecturerA.1 <- lm(lecturerA ~ Age + Gender, data=agreeLecturer)
LecturerA.2 <- lm(lecturerA ~ Age + Gender + studentN + studentE + studentO + studentA + studentC,data=agreeLecturer)
#-----Run an anova to compare the two models------
anova(LecturerA.1, LecturerA.2)
#-----To obtain output----
summary(LecturerA.1)
summary(LecturerA.2)
#----Statistics------
vif(LecturerA.2)
dwt(LecturerA.2)

#---Histogram-----
hist(rstudent(LecturerA.2))

#-----Confidence intervals-----
confint(LecturerA.2)

##-----obtain the standardized beta estimates:------

lm.beta(LecturerA.1)
lm.beta(LecturerA.2)
#-----Concientious Lecturer-----------

#----Create two models-------
LecturerC.1 <- lm(lecturerC ~ Age + Gender, data=concLecturer)
LecturerC.2 <- lm(lecturerC ~ Age + Gender + studentN + studentE + studentO + studentA + studentC,data=concLecturer)
#-----Run an anova to compare the two models------
anova(LecturerC.1, LecturerC.2)
#-----To obtain output----
summary(LecturerC.1)
summary(LecturerC.2)
#----Statistics------
vif(LecturerC.2)
dwt(LecturerC.2)

#---Histogram-----
hist(rstudent(LecturerC.2))

#-----Confidence intervals-----
confint(LecturerC.2)

##-----obtain the standardized beta estimates:------

lm.beta(LecturerC.1)
lm.beta(LecturerC.2)

#*********************Smart Alex********************

#---Task 1------
#load in the pubs.dat data:

pubs<-read.delim("pubs.dat", header = TRUE)

#create a regression model to predict mortality from number of pubs:

pubsReg <-lm(mortality ~ pubs, data = pubs)

#obtain output of the regression:

summary(pubsReg)

#--Bootstrap the regression parameters:
#first execute the bootreg() function from the book chapter.

#We can then use the function to obtain the bootstrap samples:
bootResults<-boot(statistic = bootReg, formula = mortality ~ pubs, data = pubs, R = 2000)

#Obtain the bootstrap confidence intervals for the intercept and slope:
boot.ci(bootResults, type = "bca", index = 1)

boot.ci(bootResults, type = "bca", index = 2)

#---Task 2------

#load in the Supermodel.dat data--

Supermodel<-read.delim("Supermodel.dat", header = TRUE)

#----create a regression model to predict salery from Age, number of years being a supermodel and beauty-----
Supermodel.1 <- lm(salary~age + beauty + years, data= Supermodel)

#--obtain output of the regression---

summary(Supermodel.1)

##-----obtain the standardized beta estimates:------

lm.beta(Supermodel.1)

##---is the model valid?----
vif(Supermodel.1)
1/vif(Supermodel.1)
dwt(Supermodel.1)
resid(Supermodel.1)
rstandard(Supermodel.1)

#----Histogram-----
hist(rstandard(Supermodel.1))

##---Plot of the standardized residuals----- 
plot(Supermodel.1$fitted.values,rstandard(Supermodel.1))

#---It also helps to add a horizontal line at the mean--
abline(0,0)

#To obtain some other plots, we can use the plot() function:

plot(Supermodel.1)
#----Obtain casewise diagnostics and add them to the original data 
Supermodel$cooks.distance<-cooks.distance(Supermodel.1)
Supermodel$residuals<-resid(Supermodel.1)
Supermodel$standardized.residuals <- rstandard(Supermodel.1)
Supermodel$studentized.residuals <- rstudent(Supermodel.1)
Supermodel$dfbeta <- dfbeta(Supermodel.1)
Supermodel$dffit <- dffits(Supermodel.1)
Supermodel$leverage <- hatvalues(Supermodel.1)
Supermodel$covariance.ratios <- covratio(Supermodel.1)

#----List of standardized residuals greater than 2--------------
Supermodel$standardized.residuals>2| Supermodel$standardized.residuals < -2

#---Create a variable called large.residual, which is TRUE (or 1) if the residual is greater than 2, or less than -2.----------
Supermodel$large.residual <- Supermodel$standardized.residuals > 2| Supermodel$standardized.residuals < -2

#---Count the number of large residuals-------------
sum(Supermodel$large.residual)

#-----If we want to display only some of the variables we can use:----
Supermodel[,c("salary", "age", "beauty", "years", "standardized.residuals")]

#---Display the value of salary, age, beauty, years, and the standardized residual, for those cases which have a residual greater than 2 or less than -2.-------------

Supermodel[Supermodel$large.residual,c("salary", "age", "beauty", "years", "standardized.residuals")]

#------Task 3-------------------------

#-----Read in data for Glastonbury Festival Regression----

gfr<-read.delim("GlastonburyFestivalRegression.dat", header=TRUE)

#---Create three dummy variables. Make sure you don't do this if there are missing data.---
gfr$crusty<-gfr$music=="Crusty"
gfr$metaller<-gfr$music=="Metaller"
gfr$indie.kid<-gfr$music=="Indie Kid"

#---Create a regression model---------

gfr.1 <- lm(gfr$change ~ gfr$crusty + gfr$metaller + gfr$indie.kid, data=gfr)
summary(gfr.1)

##---is the model valid?----
vif(gfr.1)
1/vif(gfr.1)

# The Durbinâ€“Watson statistic: 

dwt(gfr.1)

#----Histogram-----
hist(rstandard(gfr.1))

##---Plot of the standardized residuals----- 
plot(gfr.1$fitted.values,rstandard(gfr.1))

#---It also helps to add a horizontal line at the mean--
abline(0,0)

#To obtain some other plots, we can use the plot() function:
plot(gfr.1)

#----Obtain casewise diagnostics and add them to the original data 
gfr$cooks.distance<-cooks.distance(gfr.1)
gfr$residuals<-resid(gfr.1)
gfr$standardized.residuals<-rstandard(gfr.1)
gfr$studentized.residuals<-rstudent(gfr.1)
gfr$dfbeta<-dfbeta(gfr.1)
gfr$dffit<-dffits(gfr.1)
gfr$leverage<-hatvalues(gfr.1)
gfr$covariance.ratios<-covratio(gfr.1)

#----List of standardized residuals greater than 2--------------
gfr$standardized.residuals>2| gfr$standardized.residuals < -2

#---Create a variable called large.residual, which is TRUE (or 1) if the residual is greater than 2, or less than -2.----------
gfr$large.residual <- gfr$standardized.residuals > 2| gfr$standardized.residuals < -2

#---Count the number of large residuals-------------
sum(gfr$large.residual)

#-----If we want to display only some of the variables we can use:----
gfr[,c("change", "crusty", "metaller", "indie.kid", "standardized.residuals")]

#---Display the value of change, crusty, metaller, indie.kid, and the standardized residual, for those cases which have a residual greater than 2 or less than -2.-------------

gfr[gfr$large.residual,c("change", "crusty", "metaller", "indie.kid", "standardized.residuals")]

#------Task 4----------

#-----Read in data for Child Aggression----

ChildAggression<-read.delim("ChildAggression.dat", header = TRUE)

#---Conduct the analysis hierarhically entering parenting style and sibling aggression in the first step-------

ChildAggression.1<-lm(Aggression ~ Sibling_Aggression + Parenting_Style, data = ChildAggression)

#------And the remaining variables in a second step-----

ChildAggression.2<-lm(Aggression ~ Sibling_Aggression+Parenting_Style+ Diet + Computer_Games + Television, data=ChildAggression)

#----View the output of the two regressions---

summary(ChildAggression.1)
summary(ChildAggression.2)

#----To compare the R2 in two models, use the ANOVA command---

anova(ChildAggression.1, ChildAggression.2)

#---VIF------

vif(ChildAggression.1)
1/vif(ChildAggression.1)

vif(ChildAggression.2)
1/vif(ChildAggression.2)


#----The Durbin-Watson test is obtained with either dwt() or durbinWatsonTest()---

durbinWatsonTest(ChildAggression.1)
dwt(ChildAggression.2)

#---Histogram of standardized residuals---

hist(rstandard(ChildAggression.2))

#--Plot of residuals against fitted (predicted) values, with a flat line at the mean--
plot(ChildAggression.2$fitted.values,rstandard(ChildAggression.2))
abline(0, 0)

#---We can obtain standardized parameter estimates with the lm.beta() function---

lm.beta(ChildAggression.1)
lm.beta(ChildAggression.2)

#---Confidence intervals are obtained with the confint() function----
confint(ChildAggression.2)

#----You can round them to make life easier----
round(confint(ChildAggression.2), 2)

#To obtain some other plots, we can use the plot() function:

plot(ChildAggression.2)

#----Obtain casewise diagnostics and add them to the original data 
ChildAggression$cooks.distance<-cooks.distance(ChildAggression.2)
ChildAggression$residuals<-resid(ChildAggression.2)
ChildAggression$standardized.residuals <- rstandard(ChildAggression.2)
ChildAggression$studentized.residuals <- rstudent(ChildAggression.2)
ChildAggression$dfbeta <- dfbeta(ChildAggression.2)
ChildAggression$dffit <- dffits(ChildAggression.2)
ChildAggression$leverage <- hatvalues(ChildAggression.2)
ChildAggression$covariance.ratios <- covratio(ChildAggression.2)

#----List of standardized residuals greater than 2--------------
ChildAggression$standardized.residuals>2| ChildAggression$standardized.residuals < -2

#---Create a variable called large.residual, which is TRUE (or 1) if the residual is greater than 2, or less than -2.----------
ChildAggression$large.residual <- ChildAggression$standardized.residuals > 2| ChildAggression$standardized.residuals < -2

#---Count the number of large residuals-------------
sum(ChildAggression$large.residual)

#-----If we want to display only some of the variables we can use:----
ChildAggression[,c("Aggression", "Sibling_Aggression","Parenting_Style","Diet","Computer_Games", "Television", "standardized.residuals")]

#---Display the value of Aggression, Parenting_Style, Diet, Computer_Games and Television and the standardized residual, for those cases which have a residual greater than 2 or less than -2.-------------

ChildAggression[ChildAggression$large.residual,c("Aggression", "Sibling_Aggression","Parenting_Style","Diet","Computer_Games", "Television", "standardized.residuals")]

