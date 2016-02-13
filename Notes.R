# Handy functions in R

setwd("~/Google Drive/UC Berkeley/Courses/W203- Exploring and Analyzing Data/R Learning")

################################
# Vector creation
c(2:8) # [1] 2 3 4 5 6 7 8
rep(123, times=5) # [1] 123 123 123 123 123 # repeat!
rep(c(1,2,3), times=5) # [1] 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3
rep(c(7,8,9), times=3, each=2) # [1] 7 7 8 8 9 9 7 7 8 8 9 9 7 7 8 8 9 9

age = rnorm(100, 20, 5) # get 100 points from a normal distr with mean=20, sd=5

################################
# list, vector, array & matrix
c(2,3,4) # vector MUST be of same type
matrix(c(1,2,3,4), nrow=2, ncol=2) # matrix is a 2D array
a = array(rep(c(1,2,3,4), times=50), c(2,2,5)) # array can have >2 dimensions
list(name="Mike", age=25, company="IBM") # list can contain different types

a = c(1,2,3)
b = c(1.1, 2.1, 3.1)
l = list("integer" = a, "float" = b)
l[["integer"]] # double brackets access named item in a list

##########################
# c(), cbind() and rbind()
id = c(010,011,012,013)
weight = c(153,130,120,160)
age = c(44,43,39,40)
d = c(id,weight,age) #[1]  10  11  12  13 153 130 120 160  44  43  39  40
# Note: the data type of elements in a vector must be the same. 

# cbind combine them as columns, and return a matrix.
# Note: in a matrix, all values must have the same type
d= cbind(id, weight, age)
d
#     id weight age
#[1,] 10    153  44
#[2,] 11    130  43
#[3,] 12    120  39
#[4,] 13    160  40
colnames(d) # [1] "id"     "weight" "age" ; shows the col names of a matrix

cbind(1, age)
#        age
#[1,] 1  44
#[2,] 1  43
#[3,] 1  39
#[4,] 1  40

# rbind combine them as rows, and return a matrix
d = rbind(id,weight,age)
d
#       [,1] [,2] [,3] [,4]
#id       10   11   12   13
#weight  153  130  120  160
#age      44   43   39   40


# Get the indexes of TRUE value.
t = rep(TRUE,5)
t[2] = FALSE
which(t) #[1] 1 3 4 5

# TRUE has a value of 1
mean(t) #[1] 0.8

a = c(2,2,2,3,3,1)
b = c(3,3,3,3,3,NA)
t = b > a
sum(t) # NA
sum(t, na.rm = TRUE) # 3
length(t[!is.na(t)]) - sum(t, na.rm = TRUE) # No. of false, excluding NA

sum(a==2) # How many 2's are in the vector.

# Get random numbers from a uniform distribution
runif(4, min=-1, max=1) #[1]  0.4361306  0.2519430  0.2532553 -0.4403405

# Get a random sample of 10 integers between 1 and 100
sample(100, 4, replace=F)

# 4 random integers from 500-600, with replacement.
s = sample(500:600, 100, replace=T)
s[(order(s))] # Display the sorted result

#######################
# Operations on vectors
s = c(2,4,10)
t = c(1,3,5)

u = s[s %in% c(4,10,11)] # [1]  4 10

res1 = t < 4
res1 #[1]  TRUE  TRUE FALSE

res2 = s+t
res2 #[1]  3  7 15

res3 = s + t > 6
res3 #[1] FALSE  TRUE  TRUE

x = c(s, t) # Combine / Concatenate two vectors
bs = ifelse(x >= mean(x), "large", "small") # assign values conditionally

age = c(144, 2,1,2,4,5,1,9,-1,-2,-1, 133)

sort(unique(age)) # unique items in v, sorted
age[age < 0 | age > 120] = NA # recode invalid values to NA

s = append(s, c(1,2,3)) # extend a vector

a = c(1,2,2,3,4,9)
a[a != 2] # Remove all 2's from a vector
a[-2] # Exclude 2nd element
a[-c(2,3)] # Exclude 2nd and 3rd elements

#########################
# Operations on data frames
names = c("patrick", "jennifer", "ivy", "andrew", "vivien")
score1 = c(35,35,60,20,50)
score2 = c(40,45,80,NA,60)
score99 = c(43,45,8,1,2)
spiceness = c(1,2,4,5,2)
area = c("TKS", "TKS", "CWB", "", "LKW")
pr = data.frame(names, score1, score2, score99, spiceness, area)
pr$score99 <- NULL # Remove the score99 column
names(pr)[4] = "spicy" # Change the 4th column's name

rownames(pr) = pr$names # If we want, we can set the row names from [1..5] to the person's names

# Convert area into a factor instead.
pr$area = factor(pr$area)

# Convert names to string
pr["names"] = lapply(pr["names"], as.character)

# Convert the missing value "" into NA
pr$area[pr$area == ""] = NA
summary(pr$area) # you can see the difference.  But the "" is still there.
pr$area = factor(pr$area) # Get rid of the factor "" by re-factor it!
summary(pr$area) # you can see it's completely fixed now.

# Create a new string variable called taste
pr$taste = ifelse(pr$score2 > mean(pr$score2, na.rm=T), "Tasty", "Not good")

length(score2) # length of a vector: 5
length(score2[!is.na(score2)]) # lenght of a vector without NA
nrow(pr) # number of rows: 5
ncol(pr) # of columns: 4

######## Select rows
head(pr, n=2) # First two rows of pr
tail(pr, n=2) # Last two rows of pr
pr[1,] # First row of pr
pr[c(1:3),] # Rows 1 to 3 of pr
pr[c(2:4),] # Rows 2 to 4 of pr
pr_subset = subset(pr, subset = score1 > 40) # Rows where score1 > 40
pr_subset2 = subset(pr, subset = score1 != 60) # Rows where score1 != 60
pr[!is.na(pr$taste),] # all rows where taste != NA
pr_noNa = pr[!is.na(pr$taste),] # Filter out all rows where taste is na

######## Select items
age = c(144, 2,1,2,4,5,1,9,-1,-2,-1, 133)
age[age < 0] # Subsettng: select all entries which are negative

high_score1 = pr$score1 > 40 # high_score1 is a logical vector, with T for score 1 > 40
pr[high_score1,] # all rows where score1 > 40
pr$score1[high_score1] = pr$score1[high_score1] + 5 # we can given add 5 points to their score 1!

# Note: this returns a vector of TRUE/FALSE
!is.na(pr$taste) 
# [1]  TRUE  TRUE  TRUE FALSE  TRUE

special_scores = c(20,35)
# Rows where score1 is in [20, 35]
pr_subset = subset(pr, subset = score1 %in% special_scores) 
# Rows where score1 is not in [20, 35]
pr_subset = subset(pr, subset = !(score1 %in% special_scores)) 

# Select only certain columns from a data frame
pr[, c("names", "score1", "score2")]

# Select these 3 rows, based on index
pr[c(5,4,1),]

# Select and sorting
order(score1) # this produces the indexes of the sorted elements!
pr[order(score1),] # select the rows based on a sorted order of score1

# searching and matching
fruits = c("apple", "orange", "lemon", "grapes", "banana")
wanted = c("orange", "peach")
match(wanted, fruits) # result: [1]  2 NA
fruits %in% wanted # result: [1] FALSE  TRUE FALSE FALSE FALSE

#### Calculate means
pr_incr = pr$score2 - pr$score1
pr_incr_mean_NA = mean(pr_incr, na.rm = FALSE) # NA
pr_incr_mean_no_NA = mean(pr_incr, na.rm = TRUE) # 11.25 = (5+10+20+10)/4

########### Merge data frames
nms = c("patrick", "jennifer", "amy")
age = c(30,31,45)
ppl_info = data.frame(nms, age)
pr_merged = merge(pr, ppl_info, by.x = "names", by.y = "nms") # Join them by the "names" column.  
# only "patrick" and "jennifer" are returned.


########### Aggregate (combine multiple rows into one row, e.g. group by country)
age = c(3,2,5,1,9,10)
savings = c(50,5,70,60,50,80)
name = c("大文", "小強", "靜宜", "大雄", "技安","龐龐")
gender = c(1, 1, 2, 1, 1, 1)
people = data.frame(name, age, savings, gender)
totalSavingsByGender = aggregate(people[, c("savings")], list(Sex = people$gender), sum)
avgSavingsByGender = aggregate(people[, c("savings")], list(Sex = people$gender), mean)

#########################
# Plotting
# To install and use a package: 
# install.packages("ggplot2")
library(ggplot2)
qplot(score1, binwidth=0.5)
qplot(x=spicy, y=score1, data=pr) # a scatterplot between spicy and score1

plot = ggplot(cars, aes(dist))
plot + geom_histogram(binwidth = 5, aes(y=..count..))

# A quick way to do histogram
hist(cars$dist, breaks=10, col="red")

# A quick way to do scatterplot
scatterplot(cars$dist, cars$speed)

# A quick way to do a quartile-quartile plot; for inspecting normality.
qqplot = qplot(sample = cars$dist, stat="qq")
qqplot

plot = ggplot(cars, aes(dist, speed))
plot + geom_point() + scale_y_log10() # put the y-axis in log10 scale

plot = ggplot(cars, aes(x=speed, y=dist)) + geom_point(position="stack")
plot

plot = ggplot(diamonds, aes(x=carat, y=price))
plot + geom_point() # All points
plot = ggplot(diamonds, aes(x=carat, y=price, colour=color))
# Faceted by color, with 3 ros; syntax: facet_wrap(x ~ y)
plot + geom_point() + facet_wrap(~color, nrow=3) 

library(ggplot2)
plot = ggplot(diamonds, aes(x=color, y=price))
plot + stat_summary(fun.y = mean, geom = "bar", fill="white", color="black") +
  stat_summary(fun.data = mean_cl_normal, geom="errorbar", width=0.4)

imageDir = "~/Documents/Test/R-Image/"
ggsave(file.path(imageDir, "temp-test.jpg")) # saved the plot

#########################
# Describe some data

summary(pr_incr)

#install.packages("psych", dependencies=TRUE)
library(psych)
describe(pr_incr)

#install.packages("pastecs", dependencies=TRUE)
library(pastecs)
stat.desc(pr_incr, basic=F, norm=T)

# To avoid seeing too many decimal places, use the round() function 
round(stat.desc(cars, basic=F, norm=T), digits = 3)

# Or set the sci-penality value
# http://stackoverflow.com/questions/9397664/force-r-not-to-use-exponential-notation-e-g-e10
options(scipen=0) # default value
options(digits=7) # default value
stat.desc(cars, basic=F, norm=T)

# Get description of the price of diamonds, grouped by color
summary(diamonds)
by(diamonds$price, diamonds$color, describe)

# tabulate the frequencies between color and clarify
table(diamonds$clarity, diamonds$color)
# tabulate the frequencies between color and clarify, grouped by cut
table(diamonds$clarity, diamonds$color, diamonds$cut)

#########################
# Class
class(pr) # data.frame
class(names) # character, which means a vector(?) of character
class(score1) # numeric, which means a vector(?) of numeric


#########################
# Compare sets
A = c("Dog", "Cat", "Mouse")
B = c("Tiger","Lion","Cat")
A %in% B
# [1] FALSE  TRUE FALSE
intersect(A,B)
# [1] "Cat"
setdiff(A,B)
# [1] "Dog"   "Mouse"
setdiff(B,A)
# [1] "Tiger" "Lion" 


#####################
# Stacking data
a = c(1,3)
b = c(2,4)
c = c(-1,-2)
d = data.frame(Odd=a, Even=b, Neg=c)
d
#  Odd Even Neg
#1   1    2  -1
#2   3    4  -2

stack(d)
#       values  ind
#1      1       Odd
#2      3       Odd
#3      2       Even
#4      4       Even
#5     -1       Neg
#6     -2       Neg

# Must read http://seananderson.ca/2013/10/19/reshape.html
library(reshape2)
id = c(0, 1)
d$id = id
d
#  Odd Even Neg id
#1   1    2  -1  0
#2   3    4  -2  1

# Reshape so that id becomes the identity col, and all the other
# values are merged into 2 columns (variable, value)
melt(d, id = c("id"))
#  id variable value
#1  0      Odd     1
#2  1      Odd     3
#3  0     Even     2
#4  1     Even     4
#5  0      Neg    -1
#6  1      Neg    -2


##############################
# Plot a positive skewed pdf
# http://stackoverflow.com/questions/20254084/plot-normal-left-and-right-skewed-distribution-in-r
N <- 10000
x <- rnbinom(N, 10, .5)
hist(x, 
     xlim=c(min(x),max(x)), probability=T, nclass=max(x)-min(x)+1, 
     col='lightblue', xlab=' ', ylab=' ', axes=F,
     main='Positive Skewed')
lines(density(x,bw=1), col='red', lwd=3)

# Plot a negative skewed pdf
curve(dbeta(x,15,4),xlim=c(0,1))
title(main="posterior distrobution of p")


###########################
#
load("~/Google Drive/UC Berkeley/Courses/W203- Exploring and Analyzing Data/Assignments/GSS.Rdata")

# Levene's Test
library(car)
leveneTest(GSS$agewed, GSS$sex)

# Shapiro-Wilk's test
shapiro.test(GSS$agewed)
# Re-run the test, grouped by race
by(GSS$agewed, GSS$race, shapiro.test)


#########################
# Factors
## 8 control's, followed by 8 treatment's:
gl(n=2, k=8, labels = c("Control", "Treat"))
## 20 alternating 1s and 2s
gl(2, 1, 20)
## alternating pairs of 1s and 2s
gl(2, 2, 20)

f <- c("married", "single", "widowed", "married", "engaged", "single", "married")
f <- factor(f)
levels(f) # [1] "engaged" "married" "single"  "widowed"
f <- relevel(f, "single") # The levels of a factor are re-ordered so that single is first and the others are moved down.
levels(f) # [1] "single"  "engaged" "married" "widowed"

# on how to rename factors
# http://www.cookbook-r.com/Manipulating_data/Renaming_levels_of_a_factor/ 

############
# Ranking
a = c(23,45,18,10)
rank(a) # [1] 3 4 2 1

########################
# Two ways to look at the types of all the columns
str(diamonds)
sapply(diamonds, class)

#####################
# distribution function, quantile function, random generator

# Distribution function
# It describes the probability that a real-valued random variable X with a given 
# probability distribution (e.g. normal) will be found to have a value less than 
# or equal to x.
pnorm(q = 1.96, mean= 0, sd=1)

# Quantile function
# It's the reverse of distribution function.
qnorm(p = 0.975, mean=0, sd=1)

# Generate 10 numbers from a standard normal distribution
rnorm(n=10, mean=0, sd=1)

#########
# print something on the screen
cat(sprintf("My age is: %d", 44))

################
# lapply, sapply, replicate
a = c(1,2,3,4,5,6)

# lapply returns a list of the same length as X, each element of which is the 
# result of applying FUN to the corresponding element of X. 
lapply(a, function(x) x^2) 

# similar to lapply, but return a vector instead
sapply(a, function(x) x^2) 
sapply(c(1,3,5), function(x) x^2)

# Just repeat the expression (not function) N times, and return the result as a vector
replicate(10, sample(100, 1, replace=F))

##################
# Handle the "NA" level in a factor variable
a = c("red", "yellow", "red", "NA")
a_factor = factor(a)
summary(a_factor)
levels(a_factor)
sum(is.na(a_factor)) # display 0, which shows "NA" is treated as a factor instead of a real missing value
a_factor[a_factor == "NA"] = NA
sum(is.na(a_factor)) # It now displays 1
levels(a_factor) # But levels still contain "NA"
a_factor = droplevels(a_factor) # Let's remove the unused level!
a_factor

###################
# Keep only the rows which do not have NA in the columns of interest
a = c(2,3,4,NA,5)
b = c(1,5,NA,6,7)
c = c(NA,6,3,1,2)
d = c(2,3,4,1,2)
df = data.frame(a,b,c,d)

rows_subset = complete.cases(df$a, df$b, df$c) # we're interestd in analyzing columns a, b and c
df_subset = df[rows_subset,]

####################
# We can see how R breaks down a category variable into multiple dummy (binary) variables
a = c("red", "red", "orange", "brown", "purple", "brown", NA)
b = c(3,2,5,1,2,9, 10)
df = data.frame(b, a = factor(a))
contrasts(df$a)

####################
# Frequencies table
# http://www.statmethods.net/stats/frequencies.html
library(PASWR)
t = table(titanic3$parch, titanic3$survived) # Shows the frequency table: row=# of children; col=survived or not
t
prop.table(t,1) # Display probability table, calculated by row (sum of each row = 1)
prop.table(t,2) # Display probability table, calculated by col (sum of each col = 1)

####
# List of five 1's, followed by five 2's,....by five 6's.
l <- unlist(lapply(1:6, function(x) rep(x,5)))
l

