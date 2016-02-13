# Regression and unbiased estimator
rm(list=ls())
setwd("~/Google Drive/UC Berkeley/Courses/W203- Exploring and Analyzing Data/R Learning")

N = 1000
x = rnorm(N, mean=5, sd=2)

a0 = -2
a1 = +3

# Let's assume error is related to x
#error = x*2 + rnorm(N) # Error is correlated to x
error = rnorm(N) # Error is not correlated to x

# This is our model
y = a0 + a1 * x + error

cor.test(x, error)

D = data.frame(x=x, y=y)
m = lm(y~x, D)
summary(m)

# The estimates of a1 are incorrect if error is correlated to x
# Unbiased estimate: a1 = 3.03, intercept = -2.15
#   Biased estimate: a1 = 4.98, intercept = -1.87

# However, in reality we CANNOT observer error.