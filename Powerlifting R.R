# Load Packages
library(tidyverse)
library(lubridate)
library(DT)
library(ggplot2)
library(grid)
library(ggpubr)
library(gridExtra)

#read the data

meets = read_csv("C:/Users/David Ing/Desktop/datasets/powerlifting-database/meets.csv")
openpowerlifting = read_csv("C:/Users/David Ing/Desktop/datasets/powerlifting-database/openpowerlifting.csv")

#Powerlifting data set
Powerliftingstats = subset(openpowerlifting, 
                           select = c('Age', 'Sex', 'BestBenchKg', 'BestSquatKg', 'BestDeadliftKg', 'TotalKg'), 
                           drop = 'BestDeadliftKg' > 0, 'Age' > 0, 'BestSquatKg' > 0, 'BestBenchKg' > 0, 'TotalKg' > 0)
Powerliftingstatsclean = na.omit(Powerliftingstats[1:20000,])
Powerliftingstatsclean

#Age and Deadlift
Deadlift = ggplot(Powerliftingstatsclean, aes(x=Age, y=BestDeadliftKg)) + 
  geom_point(color = "dodgerblue3", alpha = 0.3) + 
  geom_smooth(method = 'loess', se=FALSE, color= "grey2") + 
  scale_x_continuous(limits = c(0,100),breaks = c(10,20,30,40,50,60,70,80,90,100))+ 
  scale_y_continuous(limits = c(0,400)) + 
  labs(title = "Age vs Deadlift")
Deadlift



#Female deadlift
FemaleDeadlift = ggplot(filter(Powerliftingstatsclean, Sex == 'F'), aes(x=Age, y=BestDeadliftKg))  + 
  geom_point(color = "dodgerblue3", alpha = 0.3) + 
  geom_smooth(method = 'loess', se=FALSE, color = "gray2") + 
  scale_x_continuous(limits = c(0,80),breaks = c(10,20,30,40,50,60,70,80))+ 
  scale_y_continuous(limits = c(0,300)) + 
  ggtitle("Female: Age vs Deadlift")
FemaleDeadlift


#Male Deadlift 
MaleDeadlift=ggplot(filter(Powerliftingstatsclean, Sex == 'M'), aes(x=Age, y=BestDeadliftKg))  + 
  geom_point(color = "dodgerblue3", alpha = 0.3) + 
  geom_smooth(method = 'loess', se=FALSE, color = "gray2") + 
  scale_x_continuous(limits = c(0,85),breaks = c(10,20,30,40,50,60,70,80,90))+ 
  scale_y_continuous(limits = c(0,400)) + 
  ggtitle("Male: Age vs Deadlift")                         
MaleDeadlift

#Bench Press
Bench = ggplot(Powerliftingstatsclean, aes(x=Age, y=BestBenchKg)) + 
  geom_point(color = "red2", alpha = 0.3) +
  geom_smooth(method = 'loess', se=FALSE, color = "gray2") + 
  scale_x_continuous(limits = c(0,100),breaks = c(10,20,30,40,50,60,70,80,90,100))+ 
  scale_y_continuous(limits = c(0,500)) + 
  ggtitle("Age vs Bench")
Bench

#Female Bench
FemaleBench = ggplot(filter(Powerliftingstatsclean, Sex == 'F'), aes(x=Age, y=BestBenchKg)) + 
  geom_point(color = "red2", alpha = 0.3) + 
  geom_smooth(method = 'loess', se=FALSE, color = "gray2") + 
  scale_x_continuous(limits = c(0,80),breaks = c(10,20,30,40,50,60,70,80))+ 
  scale_y_continuous(limits = c(0,300)) +
  ggtitle("Female: Age vs Benchpress")
FemaleBench

#Male Bench 
MaleBench = ggplot(filter(Powerliftingstatsclean, Sex == 'M'), aes(x=Age, y=BestBenchKg)) + 
  geom_point(color = "red2", alpha = 0.3) + 
  geom_smooth(method = 'loess', se=FALSE, color = "gray2") + 
  scale_x_continuous(limits = c(0,85),breaks = c(10,20,30,40,50,60,70,80))+ 
  scale_y_continuous(limits = c(0,500)) +
  ggtitle("Male: Age vs Benchpress")
MaleBench

#Squat
Squat = ggplot(Powerliftingstatsclean, aes(x=Age, y=BestSquatKg)) + 
  geom_point(color = "magenta3", alpha = 0.3) + 
  geom_smooth(method = 'loess', se=FALSE, color = "gray2") + 
  scale_x_continuous(limits = c(0,100),breaks = c(10,20,30,40,50,60,70,80,90,100))+ 
  scale_y_continuous(limits = c(0,500)) + 
  ggtitle("Age vs Squat")
Squat

#Female Squat
FemaleSquat = ggplot(filter(Powerliftingstatsclean, Sex == 'F'), aes(x=Age, y=BestSquatKg)) + 
  geom_point(color = "magenta3", alpha = 0.3) + 
  geom_smooth(method = 'loess', se=FALSE, color = "gray2") + 
  scale_x_continuous(limits = c(0,80),breaks = c(10,20,30,40,50,60,70,80))+ 
  scale_y_continuous(limits = c(0,500)) +
  ggtitle("Female: Age vs Squat")
FemaleSquat

#Male Squat
MaleSquat = ggplot(filter(Powerliftingstatsclean, Sex == 'M'), aes(x=Age, y=BestSquatKg)) + 
  geom_point(color = "magenta3", alpha = 0.3) + 
  geom_smooth(method = 'loess', se=FALSE, color = "gray2") + 
  scale_x_continuous(limits = c(0,85),breaks = c(10,20,30,40,50,60,70,80))+ 
  scale_y_continuous(limits = c(0,500)) +
  ggtitle("Male: Age vs Squat")
MaleSquat

#Total Kg
TotalKg = ggplot(Powerliftingstatsclean, aes(x=Age, y=TotalKg)) + 
  geom_point(color = "Green4", alpha = 0.3) + 
  geom_smooth(method = 'loess', se=FALSE, color = "gray2") + 
  scale_x_continuous(limits = c(0,85),breaks = c(10,20,30,40,50,60,70,80))+ 
  scale_y_continuous(limits = c(0,500)) +
  ggtitle("Age vs TotalKg")
TotalKg

#Men Total Kg
MaleTotalKg = ggplot(filter(Powerliftingstatsclean, Sex == 'M'), aes(x=Age, y=TotalKg)) + 
  geom_point(color = "Green4", alpha = 0.3) + 
  geom_smooth(method = 'loess', se=FALSE, color = "gray2") + 
  scale_x_continuous(limits = c(0,90),breaks = c(10,20,30,40,50,60,70,80,90))+ 
  scale_y_continuous(limits = c(0,500)) +
  ggtitle("Male: Age vs TotalKg")
MaleTotalKg

#Female TotalKg
FemaleTotalKg = ggplot(filter(Powerliftingstatsclean, Sex == 'F'), aes(x=Age, y=TotalKg)) + 
  geom_point(color = "Green4", alpha = 0.3) + 
  geom_smooth(method = 'loess', se=FALSE, color = "gray2") + 
  scale_x_continuous(limits = c(0,80),breaks = c(10,20,30,40,50,60,70,80))+ 
  scale_y_continuous(limits = c(0,500)) +
  ggtitle("Female: Age vs TotalKg")
FemaleTotalKg

#Comparing Data

MaleComparisonPage = ggarrange(MaleBench, MaleDeadlift, MaleSquat, MaleTotalKg)
MaleComparisonPage
FemaleComparisonPage = ggarrange(FemaleBench, FemaleDeadlift, FemaleSquat, FemaleTotalKg)
FemaleComparisonPage
 

