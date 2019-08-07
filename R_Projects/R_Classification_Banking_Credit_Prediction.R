#Banking Credit Classification

#See report in R_Projects_Reports to see the full project context


if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")}
pacman::p_load("caret","ROCR","lift","glmnet","MASS","e1071")
library(readxl)

credit <- read_excel("credit_data.xlsx")
new <- read_excel("new_applications.xlsx")



str(credit) # See if some data types were misclassified when importing data from CSV

# Fixing incorrectly classified data types:
credit$SEX <- as.factor(credit$SEX)
credit$EDUCATION <- as.factor(credit$EDUCATION)
credit$MARRIAGE <- as.factor(credit$MARRIAGE)
credit$PAY_1 <- as.factor(credit$PAY_1)
credit$PAY_2 <- as.factor(credit$PAY_2)
credit$PAY_3 <- as.factor(credit$PAY_3)
credit$PAY_4 <- as.factor(credit$PAY_4)
credit$PAY_5 <- as.factor(credit$PAY_5)
credit$PAY_6 <- as.factor(credit$PAY_6)
credit$default_0 <- as.factor(credit$default_0)



# Fixing incorrectly classified data types:
new$SEX <- as.factor(new$SEX)
new$EDUCATION <- as.factor(new$EDUCATION)
new$MARRIAGE <- as.factor(new$MARRIAGE)
new$PAY_1 <- as.factor(new$PAY_1)
new$PAY_2 <- as.factor(new$PAY_2)
new$PAY_3 <- as.factor(new$PAY_3)
new$PAY_4 <- as.factor(new$PAY_4)
new$PAY_5 <- as.factor(new$PAY_5)
new$PAY_6 <- as.factor(new$PAY_6)


inTrain <- createDataPartition(y = credit$default_0,p = 20000 / 24000, list = FALSE)
                               
training <- credit[ inTrain,]
testing <- credit[ -inTrain,]

training$ID <- NULL


model_logistic<-glm(default_0 ~., data=training, family="binomial"(link="logit"))

summary(model_logistic) 

model_logistic_stepwiseAIC<-stepAIC(model_logistic,direction = c("both"),trace = 1) #AIC stepwise

summary(model_logistic_stepwiseAIC) 

###Finding predicitons: probabilities and classification
logistic_probabilities<-predict(model_logistic_stepwiseAIC,newdata=testing,type="response") 
logistic_classification<-rep("1",3999)
logistic_classification[logistic_probabilities<0.3]="0" 
logistic_classification<-as.factor(logistic_classification)

###Confusion matrix  
confusionMatrix(logistic_classification,testing$default_0,positive = "1") #Display confusion matrix

####ROC Curve
logistic_ROC_prediction <- prediction(logistic_probabilities, testing$default_0)
logistic_ROC <- performance(logistic_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(logistic_ROC) #Plot ROC curve

####AUC (area under curve)
auc.tmp <- performance(logistic_ROC_prediction,"auc") #Create AUC data
logistic_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
logistic_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(logistic_probabilities, testing$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

#_____________________________________________________________________________

if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not

pacman::p_load("caret","partykit","ROCR","lift","rpart","e1071")

ctree_tree<-ctree(default_0 ~.,data=training) #Run ctree on training data
plot(ctree_tree, gp = gpar(fontsize = 8)) #Plotting the tree (adjust fontsize if needed)

ctree_probabilities<-predict(ctree_tree,newdata=testing,type="prob") #Predict probabilities
ctree_classification<-rep("1",3999)
ctree_classification[ctree_probabilities[,2]<0.5]="0" #Predict classification using 0.6073 threshold. Why 0.6073 - that's the average probability of being retained in the data. An alternative code: logistic_classification <- as.integer(logistic_probabilities > mean(testing$Retained.in.2012. == "1"))
ctree_classification<-as.factor(ctree_classification)

###Confusion matrix  
confusionMatrix(ctree_classification,testing$default_0,positive = "1")


ctree_pred_testing <- prediction(ctree_probabilities_testing[,2], testing$default_0) #Calculate errors


####AUC (area under curve)
auc.tmp <- performance(ctree_pred_testing,"auc") #Create AUC data
ctree_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
ctree_auc_testing


# RPART
# The rpart method has an important "complexity parameter", cp, which determines how big the tree is.  

CART_cp = rpart.control(cp = 0.0005)

rpart_tree<-rpart(default_0 ~.,data=training, method="class", control=CART_cp) #Run ctree on training data

printcp(rpart_tree) # Understand the relationship between the error and cp
plotcp(rpart_tree) # As a rule of thumb pick up the largest cp which does not give a substantial drop in error

prunned_rpart_tree<-prune(rpart_tree, cp=0.026) #Prun the tree. Play with cp to see how the resultant tree changes
plot(as.party(prunned_rpart_tree), type = "extended",gp = gpar(fontsize = 7)) #Plotting the tree (adjust fontsize if needed)

rpart_prediction_class<-predict(prunned_rpart_tree,newdata=testing, type="class") #Predict classification (for confusion matrix)
confusionMatrix(rpart_prediction_class,testing$default_0,positive = "1") #Display confusion matrix

rpart_probabilities_testing <-predict(prunned_rpart_tree,newdata=testing,type = "prob") #Predict probabilities
rpart_pred_testing <- prediction(rpart_probabilities_testing[,2], testing$default_0) #Calculate errors
rpart_ROC_testing <- performance(rpart_pred_testing,"tpr","fpr") #Create ROC curve data
plot(rpart_ROC_testing) #Plot ROC curve

auc.tmp <- performance(rpart_pred_testing,"auc") #Create AUC data
rpart_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
rpart_auc_testing #Display AUC value

plotLift(rpart_prediction_class,  testing$Retained.in.2012., cumulative = TRUE, n.buckets = 10) # Plot Lift chart

#_____________________________________________________________________________

if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not

pacman::p_load("caret","ROCR","lift","randomForest") #Check, and if needed install the necessary packages

model_forest <- randomForest(default_0 ~ ., data=training, 
                             importance=TRUE,proximity=TRUE,
                             cutoff = c(0.5, 0.5),type="classification") #cutoffs need to be determined for class 0 and class 1. By default 50/50, but need not be those necessarily
print(model_forest)   
plot(model_forest)
importance(model_forest)
varImpPlot(model_forest)

###Finding predicitons: probabilities and classification
forest_probabilities<-predict(model_forest,newdata=testing,type="prob") #Predict probabilities -- an array with 2 columns: for not retained (class 0) and for retained (class 1)
forest_classification<-rep("1",3999)
forest_classification[forest_probabilities[,2]<0.3]="0" #Predict classification using 0.5 threshold. Why 0.5 and not 0.6073? Use the same as in cutoff above
forest_classification<-as.factor(forest_classification)

confusionMatrix(forest_classification,testing$default_0, positive="1") #Display confusion matrix. Note, confusion matrix actually displays a better accuracy with threshold of 50%

#There is also a "shortcut" forest_prediction<-predict(model_forest,newdata=testing, type="response") 
#But it by default uses threshold of 50%: actually works better (more accuracy) on this data


####ROC Curve
forest_ROC_prediction <- prediction(forest_probabilities[,2], testing$default_0) #Calculate errors
forest_ROC <- performance(forest_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(forest_ROC) #Plot ROC curve

####AUC (area under curve)
AUC.tmp <- performance(forest_ROC_prediction,"auc") #Create AUC data
forest_AUC <- as.numeric(AUC.tmp@y.values) #Calculate AUC
forest_AUC #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(forest_probabilities[,2],  testing$Retained.in.2012., cumulative = TRUE, n.buckets = 10) # Plot Lift chart

### An alternative way is to plot a Lift curve not by buckets, but on all data points
Lift_forest <- performance(forest_ROC_prediction,"lift","rpp")
plot(Lift_forest)


#_____________________________________________________________________________

if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not

pacman::p_load("caret","ROCR","lift","xgboost") #Check, and if needed install the necessary packages

training.x <-model.matrix(default_0 ~ ., data = training)
testing.x <-model.matrix(default_0 ~ ., data = testing)

model_XGboost<-xgboost(data = data.matrix(training.x[,-1]), 
                       label = as.numeric(as.character(training$default_0)), 
                       eta = 0.1,
                       max_depth = 20, 
                       nround=50, 
                       objective = "binary:logistic")

XGboost_prediction<-predict(model_XGboost,newdata=testing.x[,-1:-2], type="response") #Predict classification (for confusion matrix)
confusionMatrix(as.factor(ifelse(XGboost_prediction>0.3,1,0)),testing$default_0,positive="1") #Display confusion matrix

####ROC Curve
XGboost_pred_testing <- prediction(XGboost_prediction, testing$default_0) #Calculate errors
XGboost_ROC_testing <- performance(XGboost_pred_testing,"tpr","fpr") #Create ROC curve data
plot(XGboost_ROC_testing) #Plot ROC curve

####AUC
auc.tmp <- performance(XGboost_pred_testing,"auc") #Create AUC data
XGboost_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
XGboost_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(XGboost_prediction, testing$Retained.in.2012., cumulative = TRUE, n.buckets = 10) # Plot Lift chart


#_____________________________________________________________________________

if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not

pacman::p_load("nnet","caret", "lift") #Check, and if needed install the necessary packages

my.grid <- expand.grid( .size = c(1,2,4),.decay = c(0.25,1,2)) # Tuning grid for Neural Net

model_NN <- train(default_0 ~.,data = training, method = "nnet", tuneGrid = my.grid, trace = TRUE, na.action =na.omit)

plot(model_NN) #Visualize the relationship between the number of layers, decay and accuracy

NN_prediction<-predict(model_NN, newdata=testing) #Predict classification 
confusionMatrix(NN_prediction,testing$default_0,positive = "1") # 72% accuracy

#ROC curve and AUC

NN_probabilities_testing <-predict(model_NN,newdata=testing,type = "prob") #Predict probabilities
NN_pred_testing <- prediction(NN_probabilities_testing[,2], testing$default_0) #Calculate errors
NN_ROC_testing <- performance(NN_pred_testing,"tpr","fpr") #Create ROC curve data
plot(NN_ROC_testing) #Plot ROC curve

auc.tmp <- performance(NN_pred_testing,"auc") #Create AUC data
NN_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
NN_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

plotLift(NN_prediction,  testing$Retained.in.2012., cumulative = TRUE, n.buckets = 10) # Plot Lift chart



############
###########

if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")}
pacman::p_load("caret","ROCR","lift","glmnet","MASS","e1071")
library(readxl)

credit <- read_excel("C:/Users/owner/Desktop/Book5.xlsx")
new <- read_excel("MMA867 A3 -- new applications.xlsx")

str(credit) # See if some data types were misclassified when importing data from CSV

# Fixing incorrectly classified data types:
credit$SEX <- as.factor(credit$SEX)
credit$EDUCATION <- as.factor(credit$EDUCATION)
credit$MARRIAGE <- as.factor(credit$MARRIAGE)
credit$PAY_1 <- as.factor(credit$PAY_1)
credit$PAY_2 <- as.factor(credit$PAY_2)
credit$PAY_3 <- as.factor(credit$PAY_3)
credit$PAY_4 <- as.factor(credit$PAY_4)
credit$PAY_5 <- as.factor(credit$PAY_5)
credit$PAY_6 <- as.factor(credit$PAY_6)
credit$default_0 <- as.factor(credit$default_0)

#convert new features to factors
credit[,42:59] <- lapply(credit[,42:59], as.factor)

#remove ID
credit$ID <- NULL

#check structure
str(credit)


inTrain <- createDataPartition(y = credit$default_0,p = 20000 / 24000, list = FALSE)
                               
training <- credit[ inTrain,]
testing <- credit[ -inTrain,]



model_logistic<-glm(default_0 ~., data=training, family="binomial"(link="logit"))

summary(model_logistic) 

model_logistic_stepwiseAIC<-stepAIC(model_logistic,direction = c("both"),trace = 1) #AIC stepwise

summary(model_logistic_stepwiseAIC) 

###Finding predicitons: probabilities and classification
logistic_probabilities<-predict(model_logistic_stepwiseAIC,newdata=testing,type="response") 
logistic_classification<-rep("1",3999)
logistic_classification[logistic_probabilities<0.5]="0" 
logistic_classification<-as.factor(logistic_classification)

###Confusion matrix  
confusionMatrix(logistic_classification,testing$default_0,positive = "1") #Display confusion matrix

####ROC Curve
logistic_ROC_prediction <- prediction(logistic_probabilities, testing$default_0)
logistic_ROC <- performance(logistic_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(logistic_ROC) #Plot ROC curve

####AUC (area under curve)
auc.tmp <- performance(logistic_ROC_prediction,"auc") #Create AUC data
logistic_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
logistic_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(logistic_probabilities, testing$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

#_____________________________________________________________________________

if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not

pacman::p_load("caret","partykit","ROCR","lift","rpart","e1071")

ctree_tree<-ctree(default_0 ~.,data=training) #Run ctree on training data
plot(ctree_tree, gp = gpar(fontsize = 8)) #Plotting the tree (adjust fontsize if needed)

ctree_probabilities<-predict(ctree_tree,newdata=testing,type="prob") #Predict probabilities
ctree_classification<-rep("1",3999)
ctree_classification[ctree_probabilities[,2]<0.5]="0" #Predict classification using 0.6073 threshold. Why 0.6073 - that's the average probability of being retained in the data. An alternative code: logistic_classification <- as.integer(logistic_probabilities > mean(testing$Retained.in.2012. == "1"))
ctree_classification<-as.factor(ctree_classification)

###Confusion matrix  
confusionMatrix(ctree_classification,testing$default_0,positive = "1")


ctree_pred_testing <- prediction(ctree_probabilities_testing[,2], testing$default_0) #Calculate errors


####AUC (area under curve)
auc.tmp <- performance(ctree_pred_testing,"auc") #Create AUC data
ctree_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
ctree_auc_testing


# RPART
# The rpart method has an important "complexity parameter", cp, which determines how big the tree is.  

CART_cp = rpart.control(cp = 0.0005)

rpart_tree<-rpart(default_0 ~.,data=training, method="class", control=CART_cp) #Run ctree on training data

printcp(rpart_tree) # Understand the relationship between the error and cp
plotcp(rpart_tree) # As a rule of thumb pick up the largest cp which does not give a substantial drop in error

prunned_rpart_tree<-prune(rpart_tree, cp=0.026) #Prun the tree. Play with cp to see how the resultant tree changes
plot(as.party(prunned_rpart_tree), type = "extended",gp = gpar(fontsize = 7)) #Plotting the tree (adjust fontsize if needed)

rpart_prediction_class<-predict(prunned_rpart_tree,newdata=testing, type="class") #Predict classification (for confusion matrix)
confusionMatrix(rpart_prediction_class,testing$default_0,positive = "1") #Display confusion matrix

rpart_probabilities_testing <-predict(prunned_rpart_tree,newdata=testing,type = "prob") #Predict probabilities
rpart_pred_testing <- prediction(rpart_probabilities_testing[,2], testing$default_0) #Calculate errors
rpart_ROC_testing <- performance(rpart_pred_testing,"tpr","fpr") #Create ROC curve data
plot(rpart_ROC_testing) #Plot ROC curve

auc.tmp <- performance(rpart_pred_testing,"auc") #Create AUC data
rpart_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
rpart_auc_testing #Display AUC value

plotLift(rpart_prediction_class,  testing$Retained.in.2012., cumulative = TRUE, n.buckets = 10) # Plot Lift chart

#_____________________________________________________________________________

if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not

pacman::p_load("caret","ROCR","lift","randomForest") #Check, and if needed install the necessary packages

model_forest <- randomForest(default_0 ~ ., data=training, 
                             importance=TRUE,proximity=TRUE,
                             cutoff = c(0.5, 0.5),type="classification") #cutoffs need to be determined for class 0 and class 1. By default 50/50, but need not be those necessarily
print(model_forest)   
plot(model_forest)
importance(model_forest)
varImpPlot(model_forest)

###Finding predicitons: probabilities and classification
forest_probabilities<-predict(model_forest,newdata=testing,type="prob") #Predict probabilities -- an array with 2 columns: for not retained (class 0) and for retained (class 1)
forest_classification<-rep("1",3999)
forest_classification[forest_probabilities[,2]<0.3]="0" #Predict classification using 0.5 threshold. Why 0.5 and not 0.6073? Use the same as in cutoff above
forest_classification<-as.factor(forest_classification)

confusionMatrix(forest_classification,testing$default_0, positive="1") #Display confusion matrix. Note, confusion matrix actually displays a better accuracy with threshold of 50%

#There is also a "shortcut" forest_prediction<-predict(model_forest,newdata=testing, type="response") 
#But it by default uses threshold of 50%: actually works better (more accuracy) on this data


####ROC Curve
forest_ROC_prediction <- prediction(forest_probabilities[,2], testing$default_0) #Calculate errors
forest_ROC <- performance(forest_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(forest_ROC) #Plot ROC curve

####AUC (area under curve)
AUC.tmp <- performance(forest_ROC_prediction,"auc") #Create AUC data
forest_AUC <- as.numeric(AUC.tmp@y.values) #Calculate AUC
forest_AUC #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(forest_probabilities[,2],  testing$Retained.in.2012., cumulative = TRUE, n.buckets = 10) # Plot Lift chart

### An alternative way is to plot a Lift curve not by buckets, but on all data points
Lift_forest <- performance(forest_ROC_prediction,"lift","rpp")
plot(Lift_forest)


#_____________________________________________________________________________

if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not

pacman::p_load("caret","ROCR","lift","xgboost") #Check, and if needed install the necessary packages

training.x <-model.matrix(default_0 ~ ., data = training)
testing.x <-model.matrix(default_0 ~ ., data = testing)

model_XGboost<-xgboost(data = data.matrix(training.x[,-1]), 
                       label = as.numeric(as.character(training$default_0)), 
                       eta = 0.1,
                       max_depth = 20, 
                       nround=50, 
                       objective = "binary:logistic")

XGboost_prediction<-predict(model_XGboost,newdata=testing.x[,-1], type="response") #Predict classification (for confusion matrix)
confusionMatrix(as.factor(ifelse(XGboost_prediction>0.3,1,0)),testing$default_0,positive="1") #Display confusion matrix

####ROC Curve
XGboost_pred_testing <- prediction(XGboost_prediction, testing$default_0) #Calculate errors
XGboost_ROC_testing <- performance(XGboost_pred_testing,"tpr","fpr") #Create ROC curve data
plot(XGboost_ROC_testing) #Plot ROC curve

####AUC
auc.tmp <- performance(XGboost_pred_testing,"auc") #Create AUC data
XGboost_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
XGboost_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(XGboost_prediction, testing$Retained.in.2012., cumulative = TRUE, n.buckets = 10) # Plot Lift chart


#_____________________________________________________________________________

if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not

pacman::p_load("nnet","caret", "lift") #Check, and if needed install the necessary packages

my.grid <- expand.grid( .size = c(1,2,4),.decay = c(0.25,1,2)) # Tuning grid for Neural Net

model_NN <- train(default_0 ~.,data = training, method = "nnet", tuneGrid = my.grid, trace = TRUE, na.action =na.omit)

plot(model_NN) #Visualize the relationship between the number of layers, decay and accuracy

NN_prediction<-predict(model_NN, newdata=testing) #Predict classification 
confusionMatrix(NN_prediction,testing$default_0,positive = "1") # 72% accuracy

#ROC curve and AUC

NN_probabilities_testing <-predict(model_NN,newdata=testing,type = "prob") #Predict probabilities
NN_pred_testing <- prediction(NN_probabilities_testing[,2], testing$default_0) #Calculate errors
NN_ROC_testing <- performance(NN_pred_testing,"tpr","fpr") #Create ROC curve data
plot(NN_ROC_testing) #Plot ROC curve

auc.tmp <- performance(NN_pred_testing,"auc") #Create AUC data
NN_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
NN_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

plotLift(NN_prediction,  testing$Retained.in.2012., cumulative = TRUE, n.buckets = 10) # Plot Lift chart


