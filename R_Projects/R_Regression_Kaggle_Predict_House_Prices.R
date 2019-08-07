#Model 1: Multiple Linear Regression (Limited Prep)

#Read the train/test file
train<-read.csv("housetrain.csv")
test<-read.csv("housetest.csv")


#Function to replace numeric NAs with median value and replace categorical NAs with not available
clean_na <- function(datafra)
{
  for(i in 1:ncol(datafra))
  {
    u <- datafra[,i]
    if(is.numeric(u))
    {
      datafra[is.na(u),i] <- median(datafra[!is.na(u),i])
    } else
    {
      u <- levels(u)[u]
      u[is.na(u)] <- "Not Available" #a NA becomes a new category
      datafra[,i] <- as.factor(u)
    }
  }
  datafra
}


#Put the features in test and train together and remove NAs
features <- rbind(train[,-c(1, ncol(train))], test[, -1])
features <- clean_na(features)

#Feature Engineering
Age <- features$YrSold - features$YearRemodAdd #modify home age
LogGrLivArea <- log10(features$GrLivArea) #log transform square footage to account for outliers
features <- cbind(features, Age, LogGrLivArea)

#Convert quality levels into numeric sequence
levels(features$KitchenQual) <- c(4,1,3,2,2)
features$KitchenQual <- as.numeric(levels(features$KitchenQual)[features$KitchenQual])
levels(features$ExterQual) <- c(4, 1, 3, 2)
features$ExterQual <- as.numeric(levels(features$ExterQual)[features$ExterQual])

anyNA(features)

#log transform sale price
LogPrice <- log10(train$SalePrice)
anyNA(LogPrice)

#Mutiple regression model
model <- lm(LogPrice ~ OverallQual + Neighborhood + LogGrLivArea + ExterQual + KitchenQual + Age + GarageCars + TotalBsmtSF + X1stFlrSF + GarageArea, data = features[1:nrow(train),])
summary(model)

#Build Prediction
LogPrice.test <- predict(model, newdata = features[(nrow(train)+1):nrow(features),])

#Build submission file
SalePrice <- 10.0**LogPrice.test
Id <- test$Id

submission <- data.frame(Id, SalePrice)
print(head(submission))

write.csv(submission, "submission1.csv", row.names = F)


#0.15662
#2984 Top 70%


########################################################

#Model 2: Multiple Linear Regression (Additional Prep)


library(dplyr)
library(tidyr)
library(dummies)
library(e1071)
library(caret)
library(Metrics)
library(corrplot)

#Read the train/test file
train<-read.csv("train.csv", stringsAsFactors = FALSE)
test<-read.csv("test.csv", stringsAsFactors = FALSE)



##1.data cleaning & preparation:
#outliers
ggplot(train,aes(y=SalePrice,x=GrLivArea))+ggtitle("Square Footage / Sale Price")+geom_point()
train[train$GrLivArea>4000&train$SalePrice<2e+05,]$GrLivArea <- mean(train$GrLivArea)%>%as.numeric
ggplot(train,aes(y=SalePrice,x=GrLivArea))+ggtitle("Square Footage / Sale Price - Outliers Removed")+geom_point()

#Sale price distribution 
ggplot(train,aes(SalePrice))+geom_histogram(fill="steelblue",color="black")
ggplot(train,aes(SalePrice))+geom_histogram(fill="steelblue",color="black")+ggtitle("Log Transformation") +scale_x_log10()

#Log transformation on sale price
train$SalePrice <- log(train$SalePrice+1)
test$SalePrice <- as.numeric(0)

#combine datasets to clean bother at same time
combi <- rbind(train,test)

#Missing values
missing_values <- train %>% summarise_all(funs(sum(is.na(.)/n())))
missing_values
missing_values <- gather(missing_values,key = "feature",value = "missing_pct")
missing_values
ggplot(missing_values,aes(x=feature,y=missing_pct))+geom_bar(stat="identity",fill="blue")+
  coord_flip()+theme_bw()


#Since it is possible the NAs represent not present as opposed to missing, rather than deleting them or replacing them with mean they are repalced with zeros
combi$LotFrontage[is.na(combi$LotFrontage)] <- 0
combi$MasVnrArea[is.na(combi$MasVnrArea)] <- 0
combi$BsmtFinSF1[is.na(combi$BsmtFinSF1)] <- 0
combi$BsmtFinSF2[is.na(combi$BsmtFinSF2)] <- 0
combi$BsmtUnfSF[is.na(combi$BsmtUnfSF)] <- 0
combi$TotalBsmtSF[is.na(combi$TotalBsmtSF)] <- 0
combi$BsmtFullBath[is.na(combi$BsmtFullBath)] <- 0
combi$BsmtHalfBath[is.na(combi$BsmtHalfBath)] <- 0
combi$GarageYrBlt[is.na(combi$GarageYrBlt)] <- 0
combi$GarageCars[is.na(combi$GarageCars)] <- 0
combi$GarageArea[is.na(combi$GarageArea)] <- 0
#Fix Typo
combi$GarageYrBlt[combi$GarageYrBlt==2207] <- 2007
combi[is.na(combi)] <- "None"

#Recode ordered categorical levels to continuous numerical sequences
combi$ExterQual<- recode(combi$ExterQual,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
combi$ExterCond<- recode(combi$ExterCond,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
combi$BsmtQual<- recode(combi$BsmtQual,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
combi$BsmtCond<- recode(combi$BsmtCond,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
combi$BsmtExposure<- recode(combi$BsmtExposure,"None"=0,"No"=1,"Mn"=2,"Av"=3,"Gd"=4)
combi$BsmtFinType1<- recode(combi$BsmtFinType1,"None"=0,"Unf"=1,"LwQ"=2,"Rec"=3,"BLQ"=4,"ALQ"=5,"GLQ"=6)
combi$BsmtFinType2<- recode(combi$BsmtFinType2,"None"=0,"Unf"=1,"LwQ"=2,"Rec"=3,"BLQ"=4,"ALQ"=5,"GLQ"=6)
combi$HeatingQC<- recode(combi$HeatingQC,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
combi$KitchenQual<- recode(combi$KitchenQual,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
combi$Functional<- recode(combi$Functional,"None"=0,"Sev"=1,"Maj2"=2,"Maj1"=3,"Mod"=4,"Min2"=5,"Min1"=6,"Typ"=7)
combi$FireplaceQu<- recode(combi$FireplaceQu,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
combi$GarageFinish<- recode(combi$GarageFinish,"None"=0,"Unf"=1,"RFn"=2,"Fin"=3)
combi$GarageQual<- recode(combi$GarageQual,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
combi$GarageCond<- recode(combi$GarageCond,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
combi$PoolQC<- recode(combi$PoolQC,"None"=0,"Po"=1,"Fa"=2,"TA"=3,"Gd"=4,"Ex"=5)
combi$Fence<- recode(combi$Fence,"None"=0,"MnWw"=1,"GdWo"=2,"MnPrv"=3,"GdPrv"=4)
#Adding a feature - Total area of basement
combi$TotalSF = combi$TotalBsmtSF + combi$X1stFlrSF + combi$X2ndFlrSF

#Creating dummy fields for non-ordered categorical features
combi_dummy <-dummy.data.frame(combi,dummy.classes = "character")

#Some renaming
combi_dummy <- rename(combi_dummy,"MSZoningC"="MSZoningC (all)")
combi_dummy <- rename(combi_dummy,"RoofMatlTarGrv"="RoofMatlTar&Grv")
combi_dummy <- rename(combi_dummy,"Exterior1stWdSdng"="Exterior1stWd Sdng")
combi_dummy <- rename(combi_dummy,"Exterior2ndBrkCmn"="Exterior2ndBrk Cmn")
combi_dummy <- rename(combi_dummy,"Exterior2ndWdSdng"="Exterior2ndWd Sdng")
combi_dummy <- rename(combi_dummy,"Exterior2ndWdShng"="Exterior2ndWd Shng")

#Perform log transformation on variables with high skewness
combi2 <- combi_dummy[,-241]
Col_class<- sapply(names(combi2),function(x){class(combi2[[x]])})
col_num <- names(Col_class[Col_class != "character"])
#Determining skew of each numric variable
skew <- sapply(col_num,function(x){skewness(combi2[[x]],na.rm = T)})
#Determine a threshold skewness and transform all variables above the treshold.
skew <- skew[skew > 0.75]
#Transform excessively skewed features with log(x + 1)
for(x in names(skew)) {
  combi_dummy[[x]] <- log(combi_dummy[[x]] + 1)
}


#Split combined data back into test,train and validation dataframes
train_dummy <- combi_dummy[1:1460,]
test_dummy <- combi_dummy[1461:2919,]
set.seed(123)
in_train <- createDataPartition(train_dummy$SalePrice,p=0.7,list=F)
new_train <- train_dummy[in_train,]
validation <- train_dummy[-in_train,]

#Models 

#Model 2: Multiple linear regression

#All predictor variables to see what will happen

mlr <-lm(formula = SalePrice ~., data = new_train) 
summary(mlr)
prediction<- predict(mlr,validation, type="response")
rmse(validation$SalePrice,prediction)
#0.1456502

#Predict against test data
pred_1 <- as.numeric(exp(predict(mlr, test_dummy))-1)

#Build Submission File
submission2 <- data.frame(Id = test$Id, SalePrice= pred_1)
write.csv(submission2, "submission20.csv", row.names = FALSE) 

#0.13401
#Rank 1960 (Top 46%)


##################################################################

#Model 3: Lasso Regularized Regression

#Lasso Model Setup
all_predictors <- subset(train,select = -c(SalePrice))
var_classes <- sapply(all_predictors,function(x)class(x))
num_classes <- var_classes[var_classes!="character"]
num_vars <- subset(train,select=names(num_classes))

#corrplot(cor(num_vars),method="number")
corrplot(cor(num_vars),method="circle")

#Building model
set.seed(123)
lasso <-cv.glmnet(as.matrix(new_train[, -241]), new_train[, 241])
prediction_4 <- predict(lasso, newx = as.matrix(validation[, -241]), s = "lambda.min")
rmse(validation$SalePrice,prediction_4)

#Predict against full test dataset
set.seed(123)
lasso <-  cv.glmnet(as.matrix(train_dummy[, -241]), train_dummy[, 241])
pred_2 <- as.numeric(exp(predict(lasso, newx = as.matrix(test_dummy[, -241]), s = "lambda.min"))-1)

#Build Submission File
submission3 <- data.frame(Id = test$Id, SalePrice= pred_2)
write.csv(submission3, "submission3.csv", row.names = FALSE) #Kaggle score 0.12600, Rank 1484 Top 35%

#0.12600
#Top 35%

##############################################################

#Model 4: Lasso Regularized Regression (Alternative Prep)

# get the require R packages
library(ggplot2)
library(plyr)
library(dplyr)
library(caret)
library(moments)
library(glmnet)
library(elasticnet)
library(knitr)

#Read the train/test file

train<-read.csv("train.csv", stringsAsFactors = FALSE)
test<-read.csv("test.csv", stringsAsFactors = FALSE)


# show the first few rows of training data
head(train)

# show dimension of training data sample
dim(head(train))

# combine train and test data for preprocessing
all_data <- rbind(select(train,MSSubClass:SaleCondition),
                  select(test,MSSubClass:SaleCondition))

# Data cleaning

# plot sale price against the log transformed sale price
df <- rbind(data.frame(version="log(price+1)",x=log(train$SalePrice + 1)),
            data.frame(version="price",x=train$SalePrice))

# plot histogram
ggplot(data=df) +
  facet_wrap(~version,ncol=2,scales="free_x") +
  geom_histogram(aes(x=x))

# log transform Sale Price 
train$SalePrice <- log(train$SalePrice + 1)

# for numeric feature with heavy skewness, perform log transformation
# first get data type for each feature
feature_classes <- sapply(names(all_data),function(x){class(all_data[[x]])})
numeric_feats <-names(feature_classes[feature_classes != "character"])

# determine skew for each numeric feature
skewed_feats <- sapply(numeric_feats,function(x){skewness(all_data[[x]],na.rm=TRUE)})

# keep only features that exceed a threshold for skewness
skewed_feats <- skewed_feats[skewed_feats > 0.75]

# transform excessively skewed features with log(x + 1)
for(x in names(skewed_feats)) {
  all_data[[x]] <- log(all_data[[x]] + 1)
}

# get names of categorical features
categorical_feats <- names(feature_classes[feature_classes == "character"])

# use caret dummyVars function for encoding for categorical features
dummies <- dummyVars(~.,all_data[categorical_feats])
categorical_1_hot <- predict(dummies,all_data[categorical_feats])
categorical_1_hot[is.na(categorical_1_hot)] <- 0  #for any level that was NA, set to zero

# for any missing values in numeric features, impute mean of that feature
numeric_df <- all_data[numeric_feats]

for (x in numeric_feats) {
  mean_value <- mean(train[[x]],na.rm = TRUE)
  all_data[[x]][is.na(all_data[[x]])] <- mean_value
}

# recombine cleaned data 
all_data <- cbind(all_data[numeric_feats],categorical_1_hot)

# create data for training and test
X_train <- all_data[1:nrow(train),]
X_test <- all_data[(nrow(train)+1):nrow(all_data),]
y <- train$SalePrice

# Model 4 - Lasso Regularized Regression

# set up caret model training parameters
# model specific training parameter
CARET.TRAIN.CTRL <- trainControl(method="repeatedcv",
                                 number=5,
                                 repeats=5,
                                 verboseIter=FALSE)


# train model
set.seed(123)  # for reproducibility
model_lasso <- train(x=X_train,y=y,
                     method="glmnet",
                     metric="RMSE",
                     maximize=FALSE,
                     trControl=CARET.TRAIN.CTRL,
                     tuneGrid=expand.grid(alpha=1,  # Lasso regression
                                          lambda=c(1,0.1,0.05,0.01,seq(0.009,0.001,-0.001),
                                                   0.00075,0.0005,0.0001)))
model_lasso

mean(model_lasso$resample$RMSE)


# extract coefficients for the best performing model
coef <- data.frame(coef.name = dimnames(coef(model_lasso$finalModel,s=model_lasso$bestTune$lambda))[[1]], 
                   coef.value = matrix(coef(model_lasso$finalModel,s=model_lasso$bestTune$lambda)))

# exclude the (Intercept) term
coef <- coef[-1,]

# print summary of model results
picked_features <- nrow(filter(coef,coef.value!=0))
not_picked_features <- nrow(filter(coef,coef.value==0))

cat("Lasso selected",picked_features,"variables and eliminated the other",
    not_picked_features,"variables\n")

## Lasso selected 108 variables and eliminated the other 180 variables

# sort coefficients in ascending order
coef <- arrange(coef,-coef.value)

# extract the top 10 and bottom 10 features
imp_coef <- rbind(head(coef,10),
                  tail(coef,10))

ggplot(imp_coef) +
  geom_bar(aes(x=reorder(coef.name,coef.value),y=coef.value),
           stat="identity") +
  ylim(-1.5,0.6) +
  coord_flip() +
  ggtitle("Top 10, Bottom 10 Features") +
  theme(axis.title=element_blank())

# predict model against test data
preds <- exp(predict(model_lasso,newdata=X_test)) - 1


# build submission file
solution <- data.frame(Id=as.integer(rownames(X_test)),SalePrice=preds)
write.csv(solution,"submission5.csv",row.names=FALSE)

#0.12391
#Rank 1348
#Top 31%




