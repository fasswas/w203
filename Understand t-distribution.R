# Understand t Distribution
rm(list=ls())
setwd("~/Google Drive/UC Berkeley/Courses/W203- Exploring and Analyzing Data/R Learning")

pop = rnorm(n=1000000, mean=0, sd=5)
hist(pop, breaks=20, freq=T)

mean.pop = mean(pop)

get.tstat = function()
{
  sample = sample(pop, n, replace=F)
  s = sd(sample)
  sm = mean(sample)
  t = (sm - mean.pop) / (s / sqrt(n))
  return(t)
}

n = 100
r100 = replicate(1000, get.tstat())
hist(r100, breaks=20, freq = T )

n = 20
r20 = replicate(1000, get.tstat())
hist(r20, breaks=20, freq = T )

n = 10
r10 = replicate(1000, get.tstat())
hist(r10, breaks=20, freq = T )

n = 5
r5 = replicate(1000, get.tstat())
hist(r5, breaks=20, freq = T )

# the above simulation didn't quite work.  :-(
