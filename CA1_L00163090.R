
#CA1 DATA ANALYSIS
#SLEEP AND STRESS ANALYSIS

#Lakshmi Sunil Menon, L00163090


#Read csv
sleep_ <- read.csv("sleep.csv", na = "")

#Structure of the data
str(sleep_)

#Installing the packages
install.packages(c("ggplot2", "ggpubr", "tidyverse", "broom", "AICcmodavg"))
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

#Renaming the variables
names(sleep_)
names(sleep_)[names(sleep_) == 'sr'] <- 'snoring_rate'
names(sleep_)[names(sleep_) == 't'] <- 'body_temperature'
names(sleep_)[names(sleep_) == 'lm'] <- 'limb_movement'
names(sleep_)[names(sleep_) == 'bo'] <- 'blood_oxygen'
names(sleep_)[names(sleep_) == 'rem'] <- 'rapid_eyemove'
names(sleep_)[names(sleep_) == 'sr.1'] <- 'sleeping_hrs'
names(sleep_)[names(sleep_) == 'hr'] <- 'heart_rate'
names(sleep_)[names(sleep_) == 'sl'] <- 'stress_level'

install.packages("moments")
library(moments)

#Determining skewness
skewness(sleep_)

#Plotting stress level
ggplot(sleep_, aes(x=stress_level)) +
  geom_bar()

#Converting stress level into a factor variable
sleep_$stress_level <-as.factor(sleep_$stress_level)
str(sleep_)

#Descriptive statistics by stress level
install.packages("psych")
library(psych)
describeBy(sleep_, sleep_$stress_level) 

#One way anova for heartrate 
anova_one_way <- aov(heart_rate~as.factor(stress_level), data = sleep_)
summary(anova_one_way)

#Tukeys test for heart rate
TukeyHSD(anova_one_way, conf.level=.99)

#One way Anova for body temperature
anova_one_way1 <- aov(body_temperature~as.factor(stress_level), data = sleep_)
summary(anova_one_way1)
#Tukeys test for body temperature
TukeyHSD(anova_one_way1, conf.level=.99)

#One way Anova for snoring rate
anova_one_way2 <- aov(snoring_rate~as.factor(stress_level), data = sleep_)
summary(anova_one_way2)
#Tukeys test for snoring rate
TukeyHSD(anova_one_way2, conf.level=.99)

#One way Anova for blood oxygen levels
anova_one_way3 <- aov(blood_oxygen~as.factor(stress_level), data = sleep_)
summary(anova_one_way3)
#Tukeys test for blood oxygen levels
TukeyHSD(anova_one_way3, conf.level=.99)

#One way Anova for rapid eye movement
anova_one_way4 <- aov(rapid_eyemove~as.factor(stress_level), data = sleep_)
summary(anova_one_way4)
#Tukeys test for rapid eye movement
TukeyHSD(anova_one_way4, conf.level=.99)

#One way Anova for sleeping hours
anova_one_way5 <- aov(sleeping_hrs~as.factor(stress_level), data = sleep_)
summary(anova_one_way5)
#Tukeys test for sleeping hours
TukeyHSD(anova_one_way5, conf.level=.99)

#Plotting heart rate and rapid eye movement
library(ggplot2)
ggplot(sleep_) +
  aes(x = heart_rate, y = rapid_eyemove) +
  geom_point(colour = "#0c4c8a") +
  theme_minimal()

#Karl pearsons correlation for heart rate and rapid eye movement
pear_corr <- cor.test(sleep_$heart_rate, sleep_$rapid_eyemove)
pear_corr

