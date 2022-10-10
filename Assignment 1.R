#Assignment 1 - 

#Question 1
setwd("/Users/STSC/Documents/courses/Somak Paul BAN 602/Assignment/assignment 1")
#Reading Data set
sales <- read.csv(file="BAN_602_Case_1.csv", header=TRUE)

#-----------------------------------------------------------------------------------------------

#Question 1: summary for each of the Quantitative variables
#summary for Time (min)

round(mean(sales$Time..min.),digits=2)
round(median(sales$Time..min.),digits=2)
round(sd(sales$Time..min.),digits=2)
round(var(sales$Time..min.),digits=2)
round(range(sales$Time..min.),digits=2)
round(min(sales$Time..min.),digits=2)
round(max(sales$Time..min.),digits=2)

#summary for Pages Viewed

round(mean(sales$Pages.Viewed),digits=2)
round(median(sales$Pages.Viewed),digits=2)
round(sd(sales$Pages.Viewed),digits=2)
round(var(sales$Pages.Viewed),digits=2)
round(range(sales$Pages.Viewed),digits=2)
round(min(sales$Pages.Viewed),digits=2)
round(max(sales$Pages.Viewed),digits=2)

#summary for Amount Spent ($)

round(mean(sales$Amount.Spent....),digits=2)
round(median(sales$Amount.Spent....),digits=2)
round(sd(sales$Amount.Spent....),digits=2)
round(var(sales$Amount.Spent....),digits=2)
round(range(sales$Amount.Spent....),digits=2)
round(min(sales$Amount.Spent....),digits=2)
round(max(sales$Amount.Spent....),digits=2)
#Or can use summary(sales)

#-----------------------------------------------------------------------------------------------

#Question 2

# Covariance between pages viewed and amount spend 
covariance <- cov(sales$Pages.Viewed, sales$Amount.Spent)
round(covariance, 3)

# Correlation between pages viewed and amount spend 
correlation <- cor(sales$Pages.Viewed, sales$Amount.Spent)
round(correlation, 3)

#Scatter Plot

plot(sales$Pages.Viewed, sales$Amount.Spent...., type = "p", xlab = "Pages Viewed", ylab="Amount Spent", main="Scatter Plot", xgap.axis = "1", ygap.axis = "1", col=c("blue"))

abline(lm(Amount.Spent.... ~ Pages.Viewed, data = sales), col = "red")

#----------------------------------------------------------------------------------------------------------------------#

#Question 3
#Histogram to display the distribution.

hist(sales$Time..min.,col = 3, xlab="Time in Minutes",sub = "Column Time(min)",ylim=c(0,20), main = "Histogram of Time Spent")

#--------------------------------------------------------------------------------------------------------------------------------------------
#Question 4

#The joint frequencies of the two categorical variables in the dataset using 
#crosstabulation. 
cross_freq1 <- table(sales$Day,sales$Browser)
cross_freq1 

cross_freq <- addmargins(table(sales$Day,sales$Browser))
cross_freq

#The frequencies as percentages of row total and percentages of column total using two separate table
#Percentages of column

col_perc = round(cross_freq1 / colSums(cross_freq1)[col(cross_freq1)] * 100,3)
col_perc

#Percentages of row total
row_perc = round(cross_freq1 / rowSums(cross_freq1)[row(cross_freq1)] * 100,3)
row_perc
#--------------------------------------------------------------------------------------------
#Question 5

#A box plot for each of the three browser types to graphically summarize the amount 
#spent by the customers by browser type
install.packages("magrittr") 
install.packages("dplyr")  
install.packages("ggplot2")
library(ggplot2)
library(dplyr)

#box plot using ggplot library
bplot = ggplot(sales, aes(x= Amount.Spent...., y= Browser, fill = Amount.Spent....)) + 
  geom_boxplot() +
  stat_summary(fun = "mean", geom = "point", shape = 8,
               size = 4, color = "Red")
bplot


#Summary table for getting the quartile values.
Summary<-boxplot(Amount.Spent....~Browser,
                 data= sales)$stats
colnames(Summary)<-c("Chrome","Firefox","Other")
rownames(Summary)<-c("Minimum","First Quartile","Median","Third Quartile","Maximum")
Summary


#---------------------------------------------------------------------------------------------------------------------#

