library(tidyverse)
library(readxl)
library(caret)

#Grocery_Data.csv
file <- read_excel(file.choose())

#break the file into test & train

train <- file[1:700,]
test <- file[701:1000,]

#make sure that between your test & train data there are no overlaps, but all observations are captured. 

#let's build our model, but only on the train data set this time. Feel free to use your 'best' model from earlier.

reg <- lm(Grocery_Bill ~ Family_Income + Family_Size, train)

#make sure you still asses & test your model!
summary(reg)
plot(reg)

#let's predict the grocery bill in our test data
pred <- predict(reg,test)

summary(pred)

#test & train comparisons

data.frame( R2 = R2(pred, test$Grocery_Bill),
            RMSE = RMSE(pred, test$Grocery_Bill),
            MAE = MAE(pred, test$Grocery_Bill))

#how do the models compare?