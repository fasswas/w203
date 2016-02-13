# Exercies for DSUR Ch. 4
rm(list=ls())
setwd("~/Google Drive/UC Berkeley/Courses/W203- Exploring and Analyzing Data/R Learning")

#######################
# FB data
facebookData = read.delim("FacebookNarcissism.dat", header = TRUE)

# View(facebookData) - Take a quick look

# Get an original idea graph
plot = ggplot(data = facebookData, aes(x=NPQC_R_Total, y=Rating))
plot + geom_point()

# Use position = "jitter" so that the dots won't overlap with each others
plot + geom_point(size=3, position = "jitter", aes(shape=Rating_Type, colour = Rating))

#######################
# Exam Anxiety data
examData = read.delim("Exam Anxiety.dat", header=TRUE)
scatter = ggplot(examData, aes(x=Anxiety, y=Exam))

# Use linear model w/o the SE (confidence band)
scatter + geom_point() + labs(x="Exam Anxiety", y="Exam Score") + 
  geom_smooth(method=lm, color="Red", se=FALSE) 

# Two regression lines, with two SE bands!
scatter + geom_point(aes(color=Gender)) + 
  labs(x="Exam Anxiety", y="Exam Score") + 
  geom_smooth(method=lm, alpha=0.1, aes(color=Gender, fill=Gender)) 

########################
# Festival Data
festivalData = read.delim("DownloadFestival.dat", header=T)

festPlot = ggplot(festivalData, aes(day1))
festPlot + geom_histogram() + geom_abline(intercept=0,slope=10) # add a diagonal line, just for fun!

# A density plot is similar to a histogram.  
# The total area of the graph under the line is 1.
festPlot + geom_density()

festBox = ggplot(festivalData, aes(x=gender, y=day1))
festBox + geom_boxplot()

# Find the outlier and fix the data
festivalData[order(festivalData$day1),]
festivalData[611,]
festivalData$day1[611] = 2.02

# Redo the plotting
festBox = ggplot(festivalData, aes(x=gender, y=day1))
festBox + geom_boxplot()

#########################
# ChickFlick.data
chickData = read.delim("ChickFlick.dat", header = T)

bar = ggplot(chickData, aes(x=film, y=arousal, color=gender))

# See individual data points
bar + geom_point() 

# Instead, let's plot the mean arousal value of each film
bar + stat_summary(fun.y = mean, geom="bar", fill="white", color="Black") 

# Plot the 95% confidence interval range, assuming normal distribution
bar + stat_summary(fun.data = mean_cl_normal, geom="pointrange") 
bar + stat_summary(fun.data = mean_cl_normal, geom="errorbar") 

# Can put both mean and 95% CI range together!
bar + stat_summary(fun.y = mean, geom="bar", fill="White", color="Black") + 
  stat_summary(fun.data = mean_cl_normal, geom="pointrange", color="Darkred") 

bar = ggplot(chickData, aes(x=film, y=arousal, fill=gender, color=gender))

# Use dodge so that the bars for the two genders won't overlap
# Set bar colors manually
bar + stat_summary(fun.y = mean, geom="bar", position="dodge")  +
  scale_fill_manual("gender", values=c("Female"="red", "Male"="blue"))

# The first width controls the width of each error bar, and the
# width inside position controls the width between two gender bars
bar + stat_summary(fun.data = mean_cl_normal, geom="errorbar", width=0.4,
                     position=position_dodge(width=0.6)) +
  labs(x="Film", y="Arousal") +
  scale_color_manual("Gender", values=c("blue", "red")) # Set legend title and colors

#################
# Hiccups data
hiccupData = read.delim("Hiccups.dat")

# Original format:
#Baseline Tongue Carotid Rectum
#1       15      9       7      2
#2       13     18       7      4
#3        9     17       5      4

# To plot the graph:
# We need all of the scores stacked up in a single column and then 
# another variable that specifies the type of intervention.
hiccups = stack(hiccupData)
names(hiccups) = c("Hiccups", "Intervention") # Given the columns better names

# Create a new factor (i.e. category) columns for the intervention
hiccups$Intervention_Factor = factor(hiccups$Intervention)

line = ggplot(hiccups, aes(x=Intervention_Factor, y=Hiccups))

# Show the mean value of each intervention factor
line + stat_summary(fun.y = mean, geom="point")

# This time connect the mean values by a line
line + stat_summary(fun.y = mean, geom="line", aes(group=1))

# Combine points, connected line, and the 95% CI based on bootstrap (ie. not normal)
line + stat_summary(fun.y = mean, geom="point") + 
  stat_summary(fun.y = mean, geom="line", aes(group=1), linetype="dashed", color='blue') +
  stat_summary(fun.data = mean_cl_boot, geom="errorbar", width=0.2)

###################
# TextMessages
textMsg = read.delim("TextMessages.dat", header=T)
head(textMsg)

# Again, convert it to long format
library(reshape)
textData = melt(textMsg, id=c("Group"))
head(textData)
names(textData) = c("Group", "Time", "Grammer_Score")
textData$Time = factor(textData$Time)

# Use color=Group so that we can see the two groups separately
line = ggplot(textData, aes(Time, Grammer_Score, colour=Group))
line + stat_summary(fun.y = mean, geom="point")

# By specifying group=Group, ggplot connect the points which belong to the same group
line + stat_summary(fun.y = mean, geom="line", aes(group=Group))

