#
# Regression part
#

#01 Sales Data.csv
SalesData<-read.csv(file.choose()) #load data
str(SalesData) #make sure that the field types are interpreted correctly (as numbers/integers, not factors)

Regression_Model<-lm(SalesData$Market.Sales..units. ~ SalesData$Test.Sales..units.) #build a model

summary(Regression_Model) #summary of the model

par(mfrow=c(1,4)) # This command sets the plot window to show 1 row of 4 plots
plot(Regression_Model) # check the model using diagnostoc plots

#
# Simulation part
#

#install.packages("ggplot2") - -- do this only once 
library(ggplot2) #package for the histogram

set.seed(467) #set random seed
trials <- 500 #how many trials to run
demand <- floor(rnorm(trials,mean=192.0,sd=58.13)) #create demand scenarios from the forecasted distribution
quantity<-200 #set quantity we want to test

#economic model
sold <- ifelse(demand>=quantity,quantity,demand)
revenue <- sold*12 
cost <- quantity*4
profit <- revenue-cost

results <- data.frame(quantity,demand,sold,revenue,cost,profit) #combine results in one dataframe

ggplot(results,aes(x=profit))+geom_histogram()+facet_wrap(~quantity) #plot the histogram of profits for the tested quantity

Expected_profit<-mean(profit) #obtain expected profit for the tested quantity

Expected_profit
