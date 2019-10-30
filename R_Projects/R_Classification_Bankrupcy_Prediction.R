#load required packages
library(mice)
library(ROSE)
library(MASS)
library(caret)
library(pROC)
library(randomForest)
library(e1071)
library(ggplot2)
library(MLmetrics)

#importy dataset
data<-read.csv("\\Bankruptcy_data_Final.csv", header = TRUE, sep = ",", strip.white=TRUE)
#check if any missing values
md.pattern(data)
set.seed(705)
#impute missing values by using MICE package, method is pmm
data1 <- mice(data,m=1,maxit=30,meth='pmm')
complete <- complete(data1,1)
md.pattern(complete)
#write.csv(complete,"B:\\modeling\\Data Science Files\\Chelsea Yao\\Bankruptcy_new.csv")

#check the proportion of bankruptcy, only around 0.1%, so use ROSE package for imbalanced data
#using the method of over-sample, so increase the rows of bankruptcy data and make the percentage of bankruptcy is 25% of full data
table(complete$BK)
prop.table(table(complete$BK))
n_legit <- 92314
new_frac_legit <- 0.75
new_n_total <- n_legit/new_frac_legit
oversample <- ovun.sample(BK ~ . ,data = complete, method = "over", N = new_n_total)
oversampled_data <- oversample$data
#write.csv(oversampled_data,"B:\\modeling\\Data Science Files\\Chelsea Yao\\Bankruptcy_rose.csv")
#oversampled_data<-read.csv("B:\\modeling\\Data Science Files\\Chelsea Yao\\Bankruptcy_rose.csv", header = TRUE, sep = ",", strip.white=TRUE)
table(oversampled_data$BK)
head(oversampled_data)

#Feature engineering part
#create dummy variables for year
#create year_1980 for year before 1990, year_1990 for 1990-1999, year_2000 for 2000-2009, year_2010 for 2010-current
oversampled_data$year_1980 <- ifelse(oversampled_data$DataYearFiscal <= 1989,1,0)
oversampled_data$year_1990 <- ifelse(oversampled_data$DataYearFiscal <= 1999 & oversampled_data$DataYearFiscal > 1989,1,0)
oversampled_data$year_2000 <- ifelse(oversampled_data$DataYearFiscal <= 2009 & oversampled_data$DataYearFiscal > 1999,1,0)
oversampled_data$year_2010 <- ifelse(oversampled_data$DataYearFiscal > 2009,1,0)

#take log transformation for Tobin's Q
#all of the values for Tobin's Q are positive, can't do log for other variables because they have negative values
hist(oversampled_data$TobinsQ)
oversampled_data$TobinsQ <- log(oversampled_data$TobinsQ)

#create bins for EPS
hist(oversampled_data$EPS)
oversampled_data$EPS1 <- ifelse(oversampled_data$EPS <= -4,1,0)
oversampled_data$EPS2 <- ifelse(oversampled_data$EPS > -4 & oversampled_data$EPS <= -0.5,1,0)
oversampled_data$EPS3 <- ifelse(oversampled_data$EPS > -0.5 & oversampled_data$EPS < 0,1,0)
oversampled_data$EPS4 <- ifelse(oversampled_data$EPS >= 0 & oversampled_data$EPS <= 1,1,0)
oversampled_data$EPS5 <- ifelse(oversampled_data$EPS > 1,1,0)

#create bins for Liquidity
hist(oversampled_data$Liquidity)
oversampled_data$Liquidity1 <- ifelse(oversampled_data$Liquidity <= -0.2,1,0)
oversampled_data$Liquidity2 <- ifelse(oversampled_data$Liquidity > -0.2 & oversampled_data$Liquidity <=0,1,0)
oversampled_data$Liquidity3 <- ifelse(oversampled_data$Liquidity > 0 & oversampled_data$Liquidity <=0.15,1,0)
oversampled_data$Liquidity4 <- ifelse(oversampled_data$Liquidity > 0.15 & oversampled_data$Liquidity <=0.35,1,0)
oversampled_data$Liquidity5 <- ifelse(oversampled_data$Liquidity > 0.35,1,0)

#create bins for profitability
hist(oversampled_data$Profitability)
oversampled_data$Profitability1 <- ifelse(oversampled_data$Profitability <= -5,1,0)
oversampled_data$Profitability2 <- ifelse(oversampled_data$Profitability > -5 & oversampled_data$Profitability <= -0.4,1,0)
oversampled_data$Profitability3 <- ifelse(oversampled_data$Profitability > -0.4 & oversampled_data$Profitability <= 0,1,0)
oversampled_data$Profitability4 <- ifelse(oversampled_data$Profitability > 0 & oversampled_data$Profitability <= 0.15,1,0)
oversampled_data$Profitability5 <- ifelse(oversampled_data$Profitability > 0.15 & oversampled_data$Profitability <= 0.4,1,0)
oversampled_data$Profitability6 <- ifelse(oversampled_data$Profitability > 0.4,1,0)

#create bins for productivity
hist(oversampled_data$Productivity)
oversampled_data$Productivity1 <- ifelse(oversampled_data$Productivity <= -0.6,1,0)
oversampled_data$Productivity2 <- ifelse(oversampled_data$Productivity > -0.6 & oversampled_data$Productivity <= -0.1,1,0)
oversampled_data$Productivity3 <- ifelse(oversampled_data$Productivity > -0.1 & oversampled_data$Productivity <= 0,1,0)
oversampled_data$Productivity4 <- ifelse(oversampled_data$Productivity > 0 & oversampled_data$Productivity <= 0.07,1,0)
oversampled_data$Productivity5 <- ifelse(oversampled_data$Productivity > 0.07 & oversampled_data$Productivity <= 0.15,1,0)
oversampled_data$Productivity6 <- ifelse(oversampled_data$Productivity > 0.15,1,0)

#create bins for leverage ratio
hist(oversampled_data$LeverageRatio)
oversampled_data$Leverage1 <- ifelse(oversampled_data$LeverageRatio <= -1,1,0)
oversampled_data$Leverage2 <- ifelse(oversampled_data$LeverageRatio > -1 & oversampled_data$LeverageRatio <=0,1,0)
oversampled_data$Leverage3 <- ifelse(oversampled_data$LeverageRatio > 0 & oversampled_data$LeverageRatio <=0.3,1,0)
oversampled_data$Leverage4 <- ifelse(oversampled_data$LeverageRatio > 0.3 & oversampled_data$LeverageRatio <=1,1,0)
oversampled_data$Leverage5 <- ifelse(oversampled_data$LeverageRatio > 1,1,0)

#create bins for asset turnover
hist(oversampled_data$AssetTurnover)
oversampled_data$AssetTurnover1 <- ifelse(oversampled_data$AssetTurnover <= 0,1,0)
oversampled_data$AssetTurnover2 <- ifelse(oversampled_data$AssetTurnover > 0 & oversampled_data$AssetTurnover <= 0.45,1,0)
oversampled_data$AssetTurnover3 <- ifelse(oversampled_data$AssetTurnover > 0.45 & oversampled_data$AssetTurnover <= 1,1,0)
oversampled_data$AssetTurnover4 <- ifelse(oversampled_data$AssetTurnover > 1 & oversampled_data$AssetTurnover <= 1.65,1,0)
oversampled_data$AssetTurnover5 <- ifelse(oversampled_data$AssetTurnover > 1.65,1,0)

#create bins for operation margin
hist(oversampled_data$OperationalMargin)
oversampled_data$Margin1 <- ifelse(oversampled_data$OperationalMargin <= -0.5,1,0)
oversampled_data$Margin2 <- ifelse(oversampled_data$OperationalMargin > -0.5 & oversampled_data$OperationalMargin <=0,1,0)
oversampled_data$Margin3 <- ifelse(oversampled_data$OperationalMargin > 0 & oversampled_data$OperationalMargin <=0.07,1,0)
oversampled_data$Margin4 <- ifelse(oversampled_data$OperationalMargin > 0.07 & oversampled_data$OperationalMargin <=0.12,1,0)
oversampled_data$Margin5 <- ifelse(oversampled_data$OperationalMargin > 0.12,1,0)

#create bins for return on equity
hist(oversampled_data$ReturnonEquity)
oversampled_data$Return1 <- ifelse(oversampled_data$ReturnonEquity <= -0.3,1,0)
oversampled_data$Return2 <- ifelse(oversampled_data$ReturnonEquity > -0.3 & oversampled_data$ReturnonEquity <=0,1,0)
oversampled_data$Return3 <- ifelse(oversampled_data$ReturnonEquity > 0 & oversampled_data$ReturnonEquity <=0.06,1,0)
oversampled_data$Return4 <- ifelse(oversampled_data$ReturnonEquity > 0.06,1,0)

#create bins for market book ratio
hist(oversampled_data$MarketBookRatio)
oversampled_data$Market1 <- ifelse(oversampled_data$MarketBookRatio <= 0,1,0)
oversampled_data$Market2 <- ifelse(oversampled_data$MarketBookRatio > 0 & oversampled_data$MarketBookRatio <= 15,1,0)
oversampled_data$Market3 <- ifelse(oversampled_data$MarketBookRatio > 15 & oversampled_data$MarketBookRatio <= 60,1,0)
oversampled_data$Market4 <- ifelse(oversampled_data$MarketBookRatio > 60 & oversampled_data$MarketBookRatio <= 230,1,0)
oversampled_data$Market5 <- ifelse(oversampled_data$MarketBookRatio > 230,1,0)

#create bins for assets growth
hist(oversampled_data$AssetsGrowth)
oversampled_data$AssetsGrowth1 <- ifelse(oversampled_data$AssetsGrowth <= -0.2,1,0)
oversampled_data$AssetsGrowth2 <- ifelse(oversampled_data$AssetsGrowth > -0.2 & oversampled_data$AssetsGrowth <= 0,1,0)
oversampled_data$AssetsGrowth3 <- ifelse(oversampled_data$AssetsGrowth > 0 & oversampled_data$AssetsGrowth <= 0.18,1,0)
oversampled_data$AssetsGrowth4 <- ifelse(oversampled_data$AssetsGrowth > 0.18,1,0)

#create bins for sales growth
hist(oversampled_data$SalesGrowth)
oversampled_data$Sales1 <- ifelse(oversampled_data$SalesGrowth <= -0.16,1,0)
oversampled_data$Sales2 <- ifelse(oversampled_data$SalesGrowth > -0.16 & oversampled_data$SalesGrowth <=0,1,0)
oversampled_data$Sales3 <- ifelse(oversampled_data$SalesGrowth > 0 & oversampled_data$SalesGrowth <=0.2,1,0)
oversampled_data$Sales4 <- ifelse(oversampled_data$SalesGrowth > 0.2,1,0)

#create bins for employee growth
hist(oversampled_data$EmployeeGrowth)
oversampled_data$Employee1 <- ifelse(oversampled_data$EmployeeGrowth <= -0.1,1,0)
oversampled_data$Employee2 <- ifelse(oversampled_data$EmployeeGrowth > -0.1 & oversampled_data$EmployeeGrowth <=0,1,0)
oversampled_data$Employee3 <- ifelse(oversampled_data$EmployeeGrowth > 0 & oversampled_data$EmployeeGrowth <=0.13,1,0)
oversampled_data$Employee4 <- ifelse(oversampled_data$EmployeeGrowth > 0.13,1,0)

#randomly select 36000 rows (around 25%) as testing data
hold_ind<-sample(seq_len(nrow(oversampled_data)), size = 36000)
test <- oversampled_data[hold_ind,]
train <- oversampled_data[-hold_ind,]

###############   Logistic  ###################
#build logistic regression, then use stepwise to select predictors
a2_glm <- glm(BK~year_1980+year_1990+year_2000+EPS1+EPS2+EPS3+EPS4+Liquidity1+Liquidity2+Liquidity3+Liquidity4+
                Profitability1+Profitability2+Profitability3+Profitability4+Profitability5+Productivity1+Productivity2+Productivity3+
                Productivity4+Productivity5+Leverage1+Leverage2+Leverage3+Leverage4+AssetTurnover1+AssetTurnover2+
                AssetTurnover3+AssetTurnover4+Margin1+Margin2+Margin3+Margin4+Return1+Return2+Return3+Market1+Market2+
                Market3+Market4+AssetsGrowth1+AssetsGrowth2+AssetsGrowth3+Sales1+Sales2+Sales3+Employee1+Employee2+Employee3,
              data=train,family = binomial)
summary(a2_glm)
a2_stepwise <- stepAIC(a2_glm,direction="both",trace=FALSE)
summary(a2_stepwise)

#predictions based on stepwise, select 0.6 as cutoff, calculate confusion matrix, F1 score, error rate, AUC
pred_stepwise <- predict(a2_stepwise,test)
pred_stepwise1 <- rep("1",36000)
pred_stepwise1[pred_stepwise<mean(pred_stepwise,0.6)]="0"
pred_stepwise1 <- as.factor(pred_stepwise1)
confusionMatrix(pred_stepwise1,as.factor(test$BK),positive="1") #acc:0.7374 sen:0.9754 spe:0.6582
F1_Score(test$BK, pred_stepwise1, positive = 1)  #0.6497
error_glm <- 1- sum(pred_stepwise1 == test$BK)/length(pred_stepwise1) #0.2626
curve_stepwise <- roc(test$BK,pred_stepwise,smooth=F,ci=T)
curve_stepwise  #0.9458
par(mfrow=c(2,2))
plot(a2_stepwise)
############## Random Forest #################
#convert y to factor, build random forest model
train$BK <- as.factor(train$BK)
test$BK <- as.factor(test$BK)
a2_rf <- randomForest(BK~
                        year_1980+year_1990+year_2000+EPS1+EPS2+EPS3+EPS4+Liquidity1+Liquidity2+Liquidity3+Liquidity4+
                        Profitability1+Profitability2+Profitability3+Profitability4+Profitability5+Productivity1+Productivity2+Productivity3+
                        Productivity4+Productivity5+Leverage1+Leverage2+Leverage3+Leverage4+AssetTurnover1+AssetTurnover2+
                        AssetTurnover3+AssetTurnover4+Margin1+Margin2+Margin3+Margin4+Return1+Return2+Return3+Market1+Market2+
                        Market3+Market4+AssetsGrowth1+AssetsGrowth2+AssetsGrowth3+Sales1+Sales2+Sales3+Employee1+Employee2+Employee3,
                      data=train,importance=TRUE,proximity=FALSE,cutoff=c(0.6,0.4),type="classification")
#view RF plot and feature importance plot, drop some least important features based on the plot
plot(a2_rf)
importance(a2_rf)
varImpPlot(a2_rf)

#final model after feature selection
a2_rf <- randomForest(BK~
                        year_1980+year_1990+year_2000+EPS1+EPS2+EPS3+EPS4+Liquidity1+Liquidity2+Liquidity3+Liquidity4+
                        Profitability1+Profitability2+Profitability3+Profitability4+Profitability5+Productivity2+
                        Productivity4+Productivity5+Leverage1+Leverage2+Leverage3+Leverage4+AssetTurnover2+
                        AssetTurnover3+AssetTurnover4+Margin1+Margin2+Margin3+Margin4+Return1+Return2+Return3+Market1+Market2+
                        Market3+AssetsGrowth1+AssetsGrowth2+AssetsGrowth3+Sales1+Sales2+Sales3+Employee1+Employee2,
                      data=train,importance=TRUE,proximity=FALSE,cutoff=c(0.6,0.4),type="classification")

#predictions based on final model, calculate confusion matrix, F1 score, error rate and AUC
pred_rf <- predict(a2_rf,test)
confusionMatrix(pred_rf,as.factor(test$BK),positive="1")
F1_Score(test$BK, pred_rf, positive = 1)  #0.9867
error_rf <- 1- sum(pred_rf == test$BK)/length(pred_rf) #0.00675
curve_rf <- roc(test$BK,as.numeric(pred_rf),smooth=F,ci=T)
curve_rf

#cross validation
#define a grid search to tune RF model, but took over 3 days to run it
#Create control function for training with 10 folds and keep 3 folds for training
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3, 
                        search='grid')

#create tunegrid with 10 values from 1:10 for mtry to tunning model
#Our train function will change number of entry variable at each split according to tunegrid
tunegrid <- expand.grid(.mtry = (1:10)) 

rf_gridsearch <- train(BK~
                         year_1980+year_1990+year_2000+EPS1+EPS2+EPS3+EPS4+Liquidity1+Liquidity2+Liquidity3+Liquidity4+
                         Profitability1+Profitability2+Profitability3+Profitability4+Profitability5+Productivity2+
                         Productivity4+Productivity5+Leverage1+Leverage2+Leverage3+Leverage4+AssetTurnover2+
                         AssetTurnover3+AssetTurnover4+Margin1+Margin2+Margin3+Margin4+Return1+Return2+Return3+Market1+Market2+
                         Market3+AssetsGrowth1+AssetsGrowth2+AssetsGrowth3+Sales1+Sales2+Sales3+Employee1+Employee2, 
                       data = train,
                       method = 'rf',
                       metric = 'Accuracy',
                       tuneGrid = tunegrid)
print(rf_gridsearch)

##################  SVM ########################
#build SVM model with all of the predictors, then rank weighted vectors, check the weight for each feature
a2_svm <- svm(BK~
                year_1980+year_1990+year_2000+EPS1+EPS2+EPS3+EPS4+Liquidity1+Liquidity2+Liquidity3+Liquidity4+
                Profitability1+Profitability2+Profitability3+Profitability4+Profitability5+Productivity1+Productivity2+Productivity3+
                Productivity4+Productivity5+Leverage1+Leverage2+Leverage3+Leverage4+AssetTurnover1+AssetTurnover2+
                AssetTurnover3+AssetTurnover4+Margin1+Margin2+Margin3+Margin4+Return1+Return2+Return3+Market1+Market2+
                Market3+Market4+AssetsGrowth1+AssetsGrowth2+AssetsGrowth3+Sales1+Sales2+Sales3+Employee1+Employee2+Employee3,
              data=train,type = 'C-classification', kernel = 'linear')
w <- t(a2_svm$coefs) %*% a2_svm$SV                 
w <- apply(w, 2, function(v){sqrt(sum(v^2))}) 
w <- sort(w, decreasing = T)
print(w)

#remove variables with less weights, build final model
#tunning the model by using different hyperparameter cost
#(auto tunning process took over 4 days to run, so I manually tried several cost and used 0.1 in final model)
tune_out <- tune(svm,BK~
                   year_1980+year_1990+year_2000+EPS1+EPS2+EPS3+EPS4+Liquidity1+Liquidity2+Liquidity3+Liquidity4+
                   Profitability1+Profitability2+Profitability3+Profitability4+Profitability5+Productivity1+Productivity2+
                   Productivity3+Productivity4+Leverage1+Leverage2+Leverage3+Leverage4+AssetTurnover1+AssetTurnover2+
                   AssetTurnover3+AssetTurnover4+Margin1+Margin2+Margin4+Return1+Return2+Return3+Market1+Market2+
                   AssetsGrowth1+AssetsGrowth3+Sales3+Employee2+Employee3,data=train,kernel = 'linear',
                 ranges =list(cost=c(0.001,0.01,0.1, 1,5,10,100)))

a2_svm <- svm(BK~
                year_1980+year_1990+year_2000+EPS1+EPS2+EPS3+EPS4+Liquidity1+Liquidity2+Liquidity3+Liquidity4+
                Profitability1+Profitability2+Profitability3+Profitability4+Profitability5+Productivity1+Productivity2+
                Productivity3+Productivity4+Leverage1+Leverage2+Leverage3+Leverage4+AssetTurnover1+AssetTurnover2+
                AssetTurnover3+AssetTurnover4+Margin1+Margin2+Margin4+Return1+Return2+Return3+Market1+Market2+
                AssetsGrowth1+AssetsGrowth3+Sales3+Employee2+Employee3,
              data=train,type = 'C-classification', kernel = 'linear', cost=0.1)
summary(a2_svm)

#predictions based on final model, calculate confusion matrix, F1 score, error rate and AUC
pred_svm <- predict(a2_svm,test)
confusionMatrix(pred_svm,as.factor(test$BK),positive="1") 
F1_Score(test$BK, pred_svm, positive = 1)   #0.7824
error_svm <- 1- sum(pred_svm == test$BK)/length(pred_svm) #0.1034
curve_svm <- roc(test$BK,as.numeric(pred_svm),smooth=F,ci=T)
curve_svm

