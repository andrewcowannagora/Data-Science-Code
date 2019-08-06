library (caret)

modelLookup("gbm")

#Each line in the above table is a parameter that can be optimized. Often there are good rules of thumb out there that would guide some reasonable levels to use. 
#For some models, these can have a dramatic effect but for others it may not make a large difference to predictive power.

#How to Use Caret for Tuning
#So generally, the only way to tune parameters is test and learn. You must adjust the parameter and measure model performance.
#
#So for decision trees, if you want to try 3 levels for the maxdepth, that's 3 models trained and backtested.
#
#For xgboost, if you want 3 levels for the 7 parameters, that's 3^7 = 2187 permutations!
#  
#  In R there are two ways to do tuning.

#Specifying a parameter in your trainControl called tuneLength. This will try up to


library(kernlab)

data(spam)

inTrain <- createDataPartition(spam$type, p = 0.70 , list = FALSE)
training <- spam[ inTrain, ]
test <- spam[ -inTrain, ]

train.control <- trainControl(
  method = "cv",
  number = 5, ## 5-fold CV
  summaryFunction = twoClassSummary, 
  classProbs = TRUE
)

mod_gbm <-  caret::train (type ~., data = training,  method = "gbm", metric="ROC", trControl = train.control, tuneLength = 3)  #Allow caret to select 5 values for each parameter


plot (mod_gbm)

###

grid <- expand.grid(n.trees=c(25,100,200), interaction.depth=c(1,3), shrinkage = c(.05,.1), n.minobsinnode=c(10,20))

mod_gbm <-  caret::train (type ~., data = training,  method = "gbm", metric="ROC", trControl = train.control, tuneGrid = grid)  #Allow caret to select 5 values for each parameter

plot (mod_gbm)

###

#When and When Not to Parameter Tune
#As you go through this please keep in mind the law of diminishing returns. 
#As you spend more and more time tuning a single model, you are not spending time doing something else 
#(talking to the business, building integration plans, building a new model, checking LinkedIn).

#How much is your time worth? How much machine time will you need to use? What's the cost of that? 
#How much of an improvement do you think you will get and will it matter to the business?

