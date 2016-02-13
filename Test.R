z <- c('a','b','c','d')
z <- c(z,'e','f')
z <- z[z != 'c']
cd <- c(1,2,3,2,7)
world <- data.frame(zon = z, code = cd)
names(world)
names(world)[1] = "zone"
world

world$multiplier = c(2,2,3,1,2)
world$codePlus <- world$code * world$multiplier
world$theDate <- as.Date(c('2015-01-02','2015-02-02','2015-05-11','2015-11-13','2015-04-27'))
sex <- c(0,0,1,1,NA)
sex <- factor(sex, levels=c(0:1), labels = c("male","female"))
world$sex <- sex
worldFirstTwo <- world[, c("zone","code")]
worldMaleOnly <- world[sex=="male" & !is.na(sex),]
worldCodeTwoMale <- subset(world, code==2 & sex=="male")

newMatrix <- as.matrix(world)


satisfactionData <- read.delim("~/Documents/Test/R/Honeymoon Period.dat")
satisfactionStacked <-stack( satisfactionData, 
                      select = c("Satisfaction_Base", "Satisfaction_6_Months", 
                                 "Satisfaction_12_Months", "Satisfaction_18_Months"))
satisfactionUnstacked <- unstack(satisfactionStacked)
satisfactionUnstacked2 <- unstack(satisfactionStacked, values ~ ind)

longData <-melt( satisfactionData, 
                         id = c("Person", "Gender"), # Any variables that do not vary over time
                         measured = # Variables that do vary over time
                           c("Satisfaction_Base", "Satisfaction_6_Months", "Satisfaction_12_Months", "Satisfaction_18_Months"))
wideData <- cast(longData, Person + Gender ~ variable, value = "value")

names = c("patrick", "jennifer", "andrew", "vivien")
score1 = c(5,7,8,12)
score2 = c(40,45,21,60)
spiceness = c(1,2,3,2)
pr = data.frame(names, score1, score2, spiceness)

mean(pr$score1)
summary(pr)

pr$stdScore1 = scale(pr$score1)
pr$stdScore2 = scale(pr$score2)
pr$totalScore = (pr$stdScore1 + pr$stdScore2)/2
pr$above_avg = pr$totalScore > 0
pr$spiceness = factor(pr$spiceness, levels=c(1,2,3), labels=c("mild","spicy","extra spicy"))
levels(pr$spiceness)
levels(pr$spiceness) = c("mild","spicy","hot")

gender = c(0,1,0,1)
gender = factor(gender, levels=c(0,1), labels=c("male","female"))
info = data.frame(names, gender)

pr_full = merge(pr, info, by = "names", all=TRUE)
pr_full


