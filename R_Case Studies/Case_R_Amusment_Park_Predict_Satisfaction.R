###################################
# Code for the Amusement Park Exercise - Short Version
# Modified from Chapman & Feit (2015) 
# 
#################################################################

####### Load data from an external link
sat.df <- read.csv("http://goo.gl/HKnl74")
# or load data from an intenal link
#sat.df <- read.csv("C:/....csv")

#################################################################
###### Preliminary Data Inspection

# basic descriptive statistics 
summary(sat.df)
plot()

#
install.packages("gpairs") # only run once
library(gpairs)
gpairs(sat.df)

#look at it manually since gpairs wasn't working for me
hist(sat.df$distance)
plot(overall~distance, data=sat.df, xlab="Distance", ylab="Overall Satisfaction")
#notice that its very left skewed

plot(overall~rides, data=sat.df, xlab="rides", ylab="Overall Satisfaction")
hist(sat.df$rides)

plot(overall~games, data=sat.df, xlab="games", ylab="Overall Satisfaction")
hist(sat.df$games)

plot(overall~wait, data=sat.df, xlab="Distance", ylab="Overall Satisfaction")
plot(overall~clean, data=sat.df, xlab="Distance", ylab="Overall Satisfaction")

# fix distance - it's skewed
sat.df$logdist <- log(sat.df$distance)

# NOTE: if using RStudio, it can be helpful to "Clear All" plots if a plot
# appears too small or too large; 
# this is a known issue in RStudio with various packages such as corrplot
#
#install.packages("corrplot") # only run once
library(corrplot)

corrplot.mixed(cor(sat.df[ , c(2, 4:8)]), upper="ellipse")

# The goal of a satisfaction drivers analysis is to discover relationships between customers'
# satisfaction with features of the service (or product) and their overall experience.
# For example, to what extent is satisfaction with the park's rides related to
# overall experience? Is the relationship strong or weak?

# Bi-variate plots

plot(overall~rides, data=sat.df, xlab="Satisfaction with Rides", ylab="Overall Satisfaction")

# Fitting a model with a single predictor
m1<-lm(overall~rides, data=sat.df)
str(m1)
m1$coefficients
# -94.962 + 1.703*95
summary(m1)

# R2 in the model fit equals the squared correlation for single predictor models
# In linear least squares regression with an estimated intercept term, R2 equals the square of the Pearson correlation coefficient between the observed y and modeled (predicted) f data values 
# Let's check that
cor(sat.df$overall, sat.df$rides)^2

# # F-statistic compares the model fit to the mean of the outcome variable
plot(overall~rides, data=sat.df,
     xlab="Satisfaction with Rides", ylab="Overall Satisfaction")
abline(m1, col='blue')

# #### - CI for rides variable...
# # confidence intervals by hand:
# 1.703 - 1.96*(0.106)
# 1.703 + 1.96*(0.106)

# not at all convenient
# let's have R do it
confint(m1)

##### Checking for Model Assumptions 
par(mfrow=c(2,2)) # partition the screen

plot(m1)

# Take a look at the outliers
sat.df[c(57, 129, 295), ]
# 

# ## Nonlinear Relationship Example
# # Simulate the dataset
#x <- rnorm(500)
#y <- x^2 + rnorm(500)
#toy.model <- lm(y~x)
# # Try it!:
#summary(toy.model)
#plot(y~x)
#abline(toy.model, col='blue')
#plot(toy.model$fitted.values, toy.model$residuals)
# 

#now we include all features into the model
m2 <- lm(overall ~ rides + games + wait + clean, data=sat.df)
summary(m2)
# Try it!: plot(m2)

# Comparing Models
summary(m1)$r.squared
summary(m2)$r.squared
summary(m1)$adj.r.squared
summary(m2)$adj.r.squared

# Superimpose the two models 

plot(sat.df$overall, fitted(m1), col='red', xlim=c(0,100), ylim=c(0,100), xlab="Actual Overall Satisfaction", ylab="Fitted Overall Satisfaction")
points(sat.df$overall, fitted(m2), col='blue')
legend("topleft", legend=c("model 1", "model 2"), 
       col=c("red", "blue"), pch=1)

# ##########################################################
##### Prediction
# ##########################################################

# Let R do it
predict(m2, sat.df[1:10,]) # predicted overall satisfaction for the first 10 observations - should give the same output as "fitted"
fitted(m2)[1:10] # regression output (predicted)

# hold-out prediction for comparison of the two models
# let's use the first 450 observations to "calibrate" our model and then try to predict satisfaction for the last 50 observations
# THINK! why are we doing this? 

#split the data into training and test sample

train.df<-sat.df[1:450,]
test.df<-sat.df[451:500,]

m1.train<-lm(overall~rides, data=train.df) # "calibrate" model1 on the training sample
m2.train <- lm(overall ~ rides + games + wait + clean, data=train.df) # "calibrate" model2 on the training sample

summary(m1.train)

m1.test<-predict(m1.train,test.df)
m2.test<-predict(m2.train,test.df)

# Compute R-squared in the test sample 
# R-squared = Explained variation / Total variation

SSE = sum((test.df$overall - m1.test)^2) # Explained variation
SST = sum((test.df$overall - mean(test.df$overall))^2) # Total Variation
Rsq=1 - SSE/SST

Rsq

cor(test.df$overall, m2.test)^2

SSE2 = sum((test.df$overall - m2.test)^2)
Rsq2=1 - SSE2/SST

Rsq2


# ##########################################################
##### standardizing
# ##########################################################

# # manually
# (sat.df$rides - mean(sat.df$rides)) / sd(sat.df$rides)
# THINK! Why do we do this? 

#In R
scale(sat.df$rides)

# create sat.std as a standardized version of sat

sat.std <- sat.df[ , -3]  # remove distance b/c we are going to use logdist instead (see lines 29-32)
sat.std[ , 3:8] <- scale(sat.std[ , 3:8])
head(sat.std) #display the first few observations 

##### Handling factors (i.e. categorical variables)
m3 <- lm(overall ~ rides + games + wait + clean + weekend + logdist + num.child, data = sat.std)
summary(m3)

summary(m3)$r.squared
summary(m2)$r.squared

# Can not treat num.child as a continuous variable b/c it's categorical.
# iow each additional child does not necessarily affect satisfaction the same way
# Need to convert it to factor

sat.std$num.child.factor <- factor(sat.std$num.child)
m4 <- lm(overall ~ rides + games + wait + clean + 
           weekend + logdist + num.child.factor, 
         data=sat.std)
summary(m4)

# What if I am not interested in the effect of each individual number of children but just the effect of coming with children? 
# THINK: Why would we want to model numchild as a two level categorical rather than multi-level categorical variable? 

sat.std$has.child <- factor(sat.std$num.child > 0)

m5 <- lm(overall ~ rides + games + wait + clean + logdist + has.child, 
         data=sat.std)
summary(m5) # Seems like our simplification did not deteriorate the fit


##### Interaction effects
m6 <- lm(overall ~ rides + games + wait + clean +
           weekend + logdist + has.child +
           rides:has.child + games:has.child + wait:has.child +
           clean:has.child +
           rides:weekend + games:weekend + wait:weekend + clean:weekend,
         data=sat.std)

summary(m6)


##### reduced model - drop insignificant terms
m7 <- lm(overall ~ rides + games + wait + clean + has.child + 
           wait:has.child,
         data=sat.std)
summary(m7)

