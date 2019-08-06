library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('class') # imputation with kNN
library('glmnet') #For ridge and lasso
library('corrplot') #for the correlation plot
library('caret') #Data Partitioning
library('coefplot')
library('gpairs')


#Assignment: Advertising Mix Model

#Answer the following questions:

#  . Does advertising lead to higher sales volume
#  . Which channels are most effective for advertising
#  . Are our customers price sensitive ?
#  . How do competitor's actions effect our sales
#  . Can analytics be used to predict the sales volume?




#adv_sales.csv
adv_sales <- read.csv(file.choose())

#Exploring the data set
head(adv_sales)
str(adv_sales)

sum(is.na(adv_sales)) #checking for missing observations

set.seed(77850) #set a random number generation seed to ensure that the split is the same everytime
inTrain <- createDataPartition(y = adv_sales$sales,
                               p = 750/1000, list = FALSE)


sales.training <- adv_sales[ inTrain,]
sales.testing <- adv_sales[ -inTrain,]
sales.training <- sales.training[,-1]
sales.testing <- sales.testing[,-1]
SST = sum((sales.testing$sales - mean(sales.testing$sales))^2) # Total Variation

# Fitting a model with price

m1.training<-lm(sales~price, data=sales.training)
summary(m1.training)$r.squared
m1.testing<-predict(m1.training,sales.testing)
SSE1 = sum((sales.testing$sales - m1.testing)^2) # Explained variation
Rsq1=1 - SSE1/SST
Rsq1
plot(sales~price, data=sales.training)
abline(m1.training, col='blue')

# Fitting a model with price and store

m2.training<-lm(sales~price+store, data=sales.training)
summary(m2.training)$r.squared
m2.testing<-predict(m2.training,sales.testing)
SSE2 = sum((sales.testing$sales - m2.testing)^2) # Explained variation
Rsq2=1 - SSE2/SST
Rsq2
plot(sales~store, data=sales.training)
par(mfrow=c(1,4))
plot(m2.training)

# Fitting a model with price, store and billboard

m3.training<-lm(sales~price+store+billboard, data=sales.training)
summary(m3.training)$r.squared
m3.testing<-predict(m3.training,sales.testing)
SSE3 = sum((sales.testing$sales - m3.testing)^2) # Explained variation
Rsq3=1 - SSE3/SST
Rsq3

plot(sales~billboard, data=sales.training)
par(mfrow=c(1,4))
plot(m3.training)

# Fitting a model with price, store, billboard, printout

m4.training<-lm(sales~price+store+billboard+printout, data=sales.training)
summary(m4.training)$r.squared
m4.testing<-predict(m4.training,sales.testing)
SSE4 = sum((sales.testing$sales - m4.testing)^2) # Explained variation
Rsq4=1 - SSE4/SST
Rsq4

plot(sales~printout, data=sales.training)
par(mfrow=c(1,4))
plot(m4.training)

# Fitting a model with price, store, billboard, printout and satisfaction

m5.training<-lm(sales~price+store+billboard+printout+sat, data=sales.training)
summary(m5.training)$r.squared
m5.testing<-predict(m5.training,sales.testing)
SSE5 = sum((sales.testing$sales - m5.testing)^2) # Explained variation
Rsq5=1 - SSE5/SST
Rsq5

plot(sales~sat, data=sales.training)
par(mfrow=c(1,4))
plot(m5.training)

# Fitting a model with price, store, billboard, printout, satisfaction and competition

m6.training<-lm(sales~price+store+billboard+printout+sat+comp, data=sales.training)
summary(m6.training)$r.squared
summary(m6.training)
m6.testing<-predict(m6.training,sales.testing)
SSE6 = sum((sales.testing$sales - m6.testing)^2) # Explained variation
Rsq6=1 - SSE6/SST
Rsq6    

plot(sales~comp, data=sales.training)
par(mfrow=c(1,4))
plot(m6.training)

#Fitting model with interactions

m7.training<-lm(sales~price+store+billboard+printout+sat+comp+
                  store:billboard+store:printout+billboard:printout, data=sales.training)
summary(m7.training)$r.squared  #0.9247
summary(m7.training)  
m7.testing<-predict(m7.training,sales.testing)
SSE7 = sum((sales.testing$sales - m7.testing)^2) # Explained variation
Rsq7=1 - SSE7/SST
Rsq7    #0.9297
par(mfrow=c(1,4))
plot(m7.training)
head(sales.testing)
gpairs(sales.training)
corrplot.mixed(cor(sales.training), upper="ellipse")


