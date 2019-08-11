#R_Classification_Kaggle_Telco_Customer_Churn


library(caret)
library(ROCR)
library(lift)
library(glmnet)
library(MASS)
library(e1071)
library(ggplot2)
library(mice)
library(tidyverse)
library(randomForest)
library(car)
library(ggcorrplot)
library(cowplot)
library(corrplot)
library(xgboost)
library(nnet)
library(rpart)
library(party)
library(survival)
library(survminer)
library(dplyr)




#Importing the file 
#WA_Fn-UseC_-Telco-Customer-Churn

telco <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")

str(telco)

telco$SeniorCitizen <- ifelse(telco$SeniorCitizen == 1, "Yes", "No")

telco$SeniorCitizen <- as.factor(telco$SeniorCitizen)

sum(is.na(telco)) #There are 11 missing observations in the data set. 
md.pattern(telco)

missingdata <- which(colSums(is.na(telco)) > 0)
sort(colSums(sapply(telco[missingdata], is.na)), decreasing = TRUE) # All the missing observations belong to Total Charges

#Getting rid of the instances with missing values.

telco <-na.omit(telco)

##### Visualizing the Target Churn #####

options(repr.plot.width = 6, repr.plot.height = 4)

telco %>% 
  group_by(Churn) %>% 
  summarise(Count = n())%>% 
  mutate(percent = prop.table(Count)*100)%>%
  ggplot(aes(reorder(Churn, -percent), percent), fill = Churn)+
  geom_col(fill = c("#E7B800", "#FC4E07"))+
  geom_text(aes(label = sprintf("%.2f%%", percent)), hjust = 0.01,vjust = -0.5, size =5)+ 
  theme_bw()+  
  xlab("Churn") + 
  ylab("Percentage")+
  ggtitle("Overall Churn Percentage")

##### Visualize the Categorical Variables #####

theme1 <- theme_bw()+
  theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5),legend.position="none")
theme2 <- theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),legend.position="none")

## Gender, Senior Citizen. Partner, Dependents, Phone Service, Multiple Lines ##

plot_grid(ggplot(telco, aes(x=gender,fill=Churn))+ geom_bar()+ theme1, 
          ggplot(telco, aes(x=SeniorCitizen,fill=Churn))+ geom_bar(position = 'fill')+theme1,
          ggplot(telco, aes(x=Partner,fill=Churn))+ geom_bar(position = 'fill')+theme1,
          ggplot(telco, aes(x=Dependents,fill=Churn))+ geom_bar(position = 'fill')+theme1,
          ggplot(telco, aes(x=PhoneService,fill=Churn))+ geom_bar(position = 'fill')+theme1,
          ggplot(telco, aes(x=MultipleLines,fill=Churn))+ geom_bar(position = 'fill')+theme_bw()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          align = "h")

## No Internet Service, Contract, Paperless Billing, Payment Method ##

plot_grid(ggplot(telco, aes(x=StreamingMovies,fill=Churn))+ 
            geom_bar(position = 'fill')+ theme1+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)), 
          ggplot(telco, aes(x=Contract,fill=Churn))+ 
            geom_bar(position = 'fill')+theme1+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(telco, aes(x=PaperlessBilling,fill=Churn))+ 
            geom_bar(position = 'fill')+theme1+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          ggplot(telco, aes(x=PaymentMethod,fill=Churn))+
            geom_bar(position = 'fill')+theme_bw()+
            scale_x_discrete(labels = function(x) str_wrap(x, width = 10)),
          align = "h")

##### Visualize the Continuous Variables #####

ggplot(data = telco, aes(MonthlyCharges, color = Churn))+
  geom_freqpoly(binwidth = 5, size = 1)

ggplot(data = telco, aes(TotalCharges, color = Churn))+
  geom_freqpoly(binwidth = 200, size = 1)

ggplot(data = telco, aes(tenure, colour = Churn))+
  geom_freqpoly(binwidth = 5, size = 1)

#Correlation Plot

options(repr.plot.width =6, repr.plot.height = 4)

telco_cor <- round(cor(telco[,c("tenure", "MonthlyCharges", "TotalCharges")]), 1)

ggcorrplot(telco_cor,  title = "Correlation")+theme(plot.title = element_text(hjust = 0.5))

telco %>%
  dplyr::select (tenure, MonthlyCharges, TotalCharges) %>%
  cor() %>%
  corrplot.mixed(upper = "color", tl.col = "navy", number.cex = 2)



#The Tenure column holds the time of Churn - so this is the response variable. This can be "futime"
#"Fustat" will tell you if an individual customer's churning time is censored"

#Survival Analysis#


newdata = telco

#create binary flag for CENSORED
telco$censor_flag <- ifelse(telco$Churn=="No", 1, 0)
str(telco)


#drop original churn factor variable (or else you'll have leakage!)
newdata = subset(newdata, select = -c(Churn) )
str(newdata)

#Now, you are prepared to create a churn object.
#That is basically a compiled version of the futime and fustat columns that can be interpreted by the survfit function

surv_object = Surv(time = newdata$tenure, event = newdata$censor_flag)

#we can also stratify by a particular group...lets caompare by Contract

fitKM <- survfit(surv_object ~ newdata$Contract, data = newdata)
ggsurvplot(fitKM, data = newdata, pval = TRUE)


#let's try something with Multiple lines
fit2 <- survfit(surv_object ~ MultipleLines, data = newdata)
ggsurvplot(fit2, data = newdata, pval = TRUE)

#Let's try something with Payment Method
fit3 <- survfit(surv_object ~ PaymentMethod, data = newdata)
ggsurvplot(fit3, data = newdata, pval = TRUE)

#Let's try something with Payment Method
fit4 <- survfit(surv_object ~ InternetService, data = newdata)
ggsurvplot(fit4, data = newdata, pval = TRUE)

#Let's try something with Payment Method
fit5 <- survfit(surv_object ~ gender, data = newdata)
ggsurvplot(fit5, data = newdata, pval = TRUE)

#These type of plot is called a forest plot. It shows so-called hazard ratios (HR) which are derived from the model for all covariates that we included in the formula in coxph

fit.coxph = coxph(surv_object ~ gender + InternetService + Contract + PaymentMethod, 
                  data = newdata)

ggforest(fit.coxph, data = newdata)

#to predict new observartions:
churn_new_subset = newdata[1:4,]
head(churn_new_subset)

churn_score_risk = predict(fit.coxph, churn_new_subset, type="lp", se.fit=TRUE)
churn_score_time = predict(fit.coxph, churn_new_subset, type="risk", se.fit=TRUE)

##### MODEL BUILDING #####

##### Feature Engineering ####


# Creating a tenure bin #

str(telco$tenure)

telco <- mutate(telco, tenure_bin = tenure)

telco$tenure_bin[telco$tenure_bin >=0 & telco$tenure_bin <= 12] <- '0-1 year'
telco$tenure_bin[telco$tenure_bin > 12 & telco$tenure_bin <= 24] <- '1-2 years'
telco$tenure_bin[telco$tenure_bin > 24 & telco$tenure_bin <= 36] <- '2-3 years'
telco$tenure_bin[telco$tenure_bin > 36 & telco$tenure_bin <= 48] <- '3-4 years'
telco$tenure_bin[telco$tenure_bin > 48 & telco$tenure_bin <= 60] <- '4-5 years'
telco$tenure_bin[telco$tenure_bin > 60 & telco$tenure_bin <= 72] <- '5-6 years'

telco$tenure_bin <- as.factor(telco$tenure_bin)

# Creating Monthly Charges Bin #

telco <- mutate(telco, MonthlyCharges_bin = MonthlyCharges)


str(telco$MonthlyCharges_bin)
telco$MonthlyCharges_bin <- as.integer(telco$MonthlyCharges_bin)
str(telco$MonthlyCharges_bin)

telco$MonthlyCharges_bin[telco$MonthlyCharges_bin >= 81 & telco$MonthlyCharges_bin <= 120] <- 'High'
telco$MonthlyCharges_bin[telco$MonthlyCharges_bin >= 41 & telco$MonthlyCharges_bin <= 80] <- 'Standard'
telco$MonthlyCharges_bin[telco$MonthlyCharges_bin >=0 & telco$MonthlyCharges_bin <= 40] <- 'Low'

telco$MonthlyCharges_bin <- as.factor(telco$MonthlyCharges_bin)


#Total Charges Bin

telco <- mutate(telco, TotalCharges_bin = TotalCharges)

str(telco$TotalCharges_bin)

telco$TotalCharges_bin[telco$TotalCharges_bin >= 0 & telco$TotalCharges_bin <= 3000] <- 'Standard'
telco$TotalCharges_bin[telco$TotalCharges_bin > 3000 & telco$TotalCharges_bin <= 6001] <- 'Medium'
telco$TotalCharges_bin[telco$TotalCharges_bin > 6001 & telco$TotalCharges_bin <= 9000] <- 'High'

telco$TotalCharges_bin <- as.factor(telco$TotalCharges_bin)

# Monthly charges as percentage of Total Charges #

telco <- mutate(telco, monthlychargespercentageoftotal = (MonthlyCharges/TotalCharges)*100)

# Average Monthly Bill

telco <- mutate(telco, averagemonthlybill = (TotalCharges/tenure))


##### Recoding qualitative Variables ####

telco$gender <- as.factor(telco$gender)
telco$SeniorCitizen <- as.factor(telco$SeniorCitizen)
telco$Partner <- as.factor(telco$Partner)
telco$Dependents <- as.factor(telco$Dependents)
telco$PhoneService <- as.factor(telco$PhoneService)
telco$MultipleLines <- as.factor(telco$MultipleLines)
telco$PaperlessBilling <- as.factor(telco$PaperlessBilling)

telco$Churn <- ifelse(telco$Churn == "Yes", 1, 0)
telco$Churn <- as.factor(telco$Churn)

telco$InternetService <- as.factor(telco$InternetService)

#Payment Method

telco$PaymentMethod <- as.factor(telco$PaymentMethod)


str(telco)

#(telco, "Clean Data Telco.csv")

##### Scaling continuous variables #####


#tenure

telco <- mutate(telco, tenure_scaled = tenure)

telco$tenure_scaled <- scale(telco$tenure)

#Monthly Charges

telco <- mutate(telco, MonthlyCharges_scaled = MonthlyCharges)

telco$MonthlyCharges_scaled <- scale(telco$MonthlyCharges)

#Total charges

telco <- mutate(telco, TotalCharges_scaled = TotalCharges)

telco$TotalCharges_scaled <- scale(telco$TotalCharges)

#Average Monthly Charge

telco <- mutate(telco, averagemonthlybill_scaled = averagemonthlybill)

telco$averagemonthlybill_scaled <- scale(telco$averagemonthlybill)


#Dropping Unscaled Features

telco <- subset(telco, select = -c(`tenure`, `MonthlyCharges`, `TotalCharges`, `averagemonthlybill`))


# Create Training, Testing/Holdout Data set

set.seed(77850)


inTrain <- createDataPartition(y = telco$Churn,
                               p = 4923/7032, list = FALSE)

training <- telco[inTrain,]

testing <-telco[-inTrain,]

windows(10,10)

#write.csv(training, "train data.csv")
#write.csv(testing, "test data.csv")

#######Model Logistic########

model_logistic<-glm(Churn ~ gender + SeniorCitizen + Partner + Dependents + PhoneService + MultipleLines
                    + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport
                    + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod
                    + tenure_bin + MonthlyCharges_bin + TotalCharges_bin + monthlychargespercentageoftotal
                    + averagemonthlybill_scaled + tenure_scaled + MonthlyCharges_scaled + TotalCharges_scaled, data=training, family="binomial"(link="logit"))

summary(model_logistic) 

logistic_probabilities <- predict(model_logistic,newdata=testing,type="response") 
logistic_classification <- rep("1",2108)
logistic_classification[logistic_probabilities < 0.26] = "0" 
logistic_classification<-as.factor(logistic_classification)


logistic_testing_results <- cbind(logistic_probabilities, logistic_classification)
#write.csv(logistic_testing_results, "logistic_testing_model_results.csv", row.names = TRUE)

###Confusion matrix  
confusionMatrix(logistic_classification,testing$Churn,positive = "1") #Display confusion matrix

windows(12,12)
####ROC Curve
logistic_ROC_prediction <- prediction(logistic_probabilities, testing$Churn)
logistic_ROC <- performance(logistic_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(logistic_ROC) #Plot ROC curve

####AUC (area under curve)
auc.tmp <- performance(logistic_ROC_prediction,"auc") #Create AUC data
logistic_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
logistic_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(logistic_probabilities, testing$Churn, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

#####Step Wise #####

model_logistic_stepwiseAIC<-stepAIC(model_logistic,direction = c("both"),trace = 1) #AIC stepwise
summary(model_logistic_stepwiseAIC) 

par(mfrow=c(1,4))
plot(model_logistic_stepwiseAIC) #Error plots: similar nature to lm plots
par(mfrow=c(1,1))

###Finding predicitons: probabilities and classification
step_logistic_probabilities <- predict(model_logistic_stepwiseAIC,newdata=testing,type="response")
step_logistic_classification <- rep("1",2108)
step_logistic_classification[step_logistic_probabilities < 0.26] = "0" 
step_logistic_classification<-as.factor(step_logistic_classification)

step_logistic_testing_results <- cbind(step_logistic_probabilities, step_logistic_classification)
#write.csv(step_logistic_testing_results, "stepwise_testing_results.csv",row.names = TRUE)


windows(10,10)
####ROC Curve
step_logistic_ROC_prediction <- prediction(step_logistic_probabilities, testing$Churn)
step_logistic_ROC <- performance(step_logistic_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(step_logistic_ROC) #Plot ROC curve

###Confusion matrix  
confusionMatrix(step_logistic_classification,testing$Churn,positive = "1") #Display confusion matrix

####AUC (area under curve)
auc.tmp <- performance(step_logistic_ROC_prediction,"auc") #Create AUC data
step_logistic_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
step_logistic_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(step_logistic_probabilities, testing$Churn, cumulative = TRUE, n.buckets = 10) # Plot Lift chart


#####CTree#####

library(partykit)
ctree_tree<-ctree(Churn ~ gender + SeniorCitizen + Partner + Dependents + PhoneService + MultipleLines
                  + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport
                  + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod
                  + tenure_bin + MonthlyCharges_bin + TotalCharges_bin + monthlychargespercentageoftotal
                  + averagemonthlybill_scaled + tenure_scaled + MonthlyCharges_scaled + TotalCharges_scaled, data=training) #Run ctree on training data

windows(20,20)
plot(ctree_tree, gp = gpar(fontsize = 8)) #Plotting the tree (adjust fontsize if needed)

ctree_probabilities<-predict(ctree_tree,newdata=testing,type="prob") #Predict probabilities
ctree_classification<-rep("1",2108)
ctree_classification[ctree_probabilities[,2]<0.26]="0" #Predict classification using 0.6073 threshold. Why 0.6073 - that's the average probability of being retained in the data. An alternative code: logistic_classification <- as.integer(logistic_probabilities > mean(testing$Retained.in.2012. == "1"))
ctree_classification<-as.factor(ctree_classification)

ctree_testing_results <- cbind(ctree_probabilities, ctree_classification)
#write.csv(ctree_testing_results, "ctree_testing_results.csv",row.names = TRUE)



###Confusion matrix  
confusionMatrix(ctree_classification,testing$Churn,positive = "1")

####ROC Curve
ctree_probabilities_testing <-predict(ctree_tree,newdata=testing,type = "prob") #Predict probabilities
ctree_pred_testing <- prediction(ctree_probabilities_testing[,2], testing$Churn) #Calculate errors
ctree_ROC_testing <- performance(ctree_pred_testing,"tpr","fpr") #Create ROC curve data
plot(ctree_ROC_testing) #Plot ROC curve

####AUC (area under curve)
auc.tmp <- performance(ctree_pred_testing,"auc") #Create AUC data
ctree_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
ctree_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(ctree_probabilities[,2],  testing$Churn, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

##### RPART#####

# The rpart method has an important "complexity parameter", cp, which determines how big the tree is.  

CART_cp = rpart.control(cp = 0.0005)

rpart_tree<-rpart(Churn ~ gender + SeniorCitizen + Partner + Dependents + PhoneService + MultipleLines
                  + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport
                  + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod
                  + tenure_bin + MonthlyCharges_bin + TotalCharges_bin + monthlychargespercentageoftotal
                  + averagemonthlybill_scaled + tenure_scaled + MonthlyCharges_scaled + TotalCharges_scaled ,data=training, method="class", control=CART_cp) #Run ctree on training data

printcp(rpart_tree) # Understand the relationship between the error and cp
plotcp(rpart_tree) # As a rule of thumb pick up the largest cp which does not give a substantial drop in error

prunned_rpart_tree<-prune(rpart_tree, cp=0.002) #Prun the tree. Play with cp to see how the resultant tree changes
plot(as.party(prunned_rpart_tree), type = "extended",gp = gpar(fontsize = 7)) #Plotting the tree (adjust fontsize if needed)

rpart_prediction_class<-predict(prunned_rpart_tree,newdata=testing, type="class") #Predict classification (for confusion matrix)
confusionMatrix(rpart_prediction_class,testing$Churn,positive = "1") #Display confusion matrix

rpart_probabilities_testing <-predict(prunned_rpart_tree,newdata=testing,type = "prob") #Predict probabilities
rpart_pred_testing <- prediction(rpart_probabilities_testing[,2], testing$Churn) #Calculate errors
rpart_ROC_testing <- performance(rpart_pred_testing,"tpr","fpr") #Create ROC curve data
plot(rpart_ROC_testing) #Plot ROC curve

#rpart_prediction_class <- predict(prunned_rpart_tree,newdata=testing, type="class")
rpart_classification <- rep("1",2108)
rpart_classification[rpart_probabilities_testing[,2] < 0.26] = "0" 


rpart_testing_results <- cbind(rpart_probabilities_testing, rpart_classification)
#write.csv(rpart_testing_results, "rpart_testing_results.csv",row.names = TRUE)


auc.tmp <- performance(rpart_pred_testing,"auc") #Create AUC data
rpart_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
rpart_auc_testing #Display AUC value

plotLift(rpart_prediction_class,  testing$Churn, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

######Random Forest########

model_forest <- randomForest(Churn ~ gender + SeniorCitizen + Partner + Dependents + PhoneService + MultipleLines
                             + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport
                             + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod
                             + tenure_bin + MonthlyCharges_bin + TotalCharges_bin + monthlychargespercentageoftotal
                             + averagemonthlybill_scaled + tenure_scaled + MonthlyCharges_scaled + TotalCharges_scaled , 
                             data=training, 
                             importance=TRUE,proximity=TRUE,
                             cutoff = c(0.70, 0.30),type="classification") 
windows(10,10)
plot(model_forest)
importance(model_forest)
varImpPlot(model_forest)

forest_probabilities<-predict(model_forest,newdata=testing,type="prob") #Predict probabilities -- an array with 2 columns: for not retained (class 0) and for retained (class 1)
forest_classification<-rep("1",2108)
forest_classification[forest_probabilities[,2]<0.26]="0"  # Anything less than 26%, people are not churning.#Predict classification using 0.5 threshold. Why 0.5 and not 0.6073? Use the same as in cutoff above
forest_classification<-as.factor(forest_classification)

confusionMatrix(forest_classification,testing$Churn , positive="1") #Display confusion matrix. Note, confusion matrix actually displays a better accuracy with threshold of 50%

####ROC Curve
forest_ROC_prediction <- prediction(forest_probabilities[,2], testing$Churn) #Calculate errors
forest_ROC <- performance(forest_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(forest_ROC) #Plot ROC curve

####AUC (area under curve)
AUC.tmp <- performance(forest_ROC_prediction,"auc") #Create AUC data
forest_AUC <- as.numeric(AUC.tmp@y.values) #Calculate AUC
forest_AUC #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(forest_probabilities[,2],  testing$Churn, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

### An alternative way is to plot a Lift curve not by buckets, but on all data points
#Lift_forest <- performance(forest_ROC_prediction,"lift","rpp")
#plot(Lift_forest)

forest_testing_results <- cbind(forest_probabilities, forest_classification)
#write.csv(forest_testing_results, "forest_model_testing_results.csv",row.names = TRUE)


######XGBoost#######

training.x <-model.matrix(Churn ~ gender + SeniorCitizen + Partner + Dependents + PhoneService + MultipleLines
                          + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport
                          + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod
                          + tenure_bin + MonthlyCharges_bin + TotalCharges_bin + monthlychargespercentageoftotal
                          + averagemonthlybill_scaled + tenure_scaled + MonthlyCharges_scaled + TotalCharges_scaled , data = training)
testing.x <-model.matrix(Churn ~ gender + SeniorCitizen + Partner + Dependents + PhoneService + MultipleLines
                         + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport
                         + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod
                         + tenure_bin + MonthlyCharges_bin + TotalCharges_bin + monthlychargespercentageoftotal
                         + averagemonthlybill_scaled + tenure_scaled + MonthlyCharges_scaled + TotalCharges_scaled, data = testing)


model_XGboost<-xgboost(data = data.matrix(training.x[,-1]), 
                       label = as.numeric(as.character(training$Churn)), 
                       eta = 0.1,
                       max_depth = 20, 
                       nround=50, 
                       objective = "binary:logistic")

XGboost_testing<-predict(model_XGboost,newdata=testing.x[,-1], type="response") #Predict classification (for confusion matrix)

XGboost_classification <- rep("1",2108)
XGboost_classification[XGboost_testing < 0.26] = "0" 

XGboost_prediction<-predict(model_XGboost,newdata=testing.x[,-1], type="response") #Predict classification (for confusion matrix)

confusionMatrix(as.factor(ifelse(XGboost_prediction< 0.26,0,1)),testing$Churn, positive="1") #Display confusion matrix

XGboost_testing_results <- cbind(XGboost_testing, XGboost_classification)
#write.csv(XGboost_testing_results, "XGboost_testing_results.csv")



####ROC Curve
XGboost_pred_testing <- prediction(XGboost_prediction, testing$Churn) #Calculate errors
XGboost_ROC_testing <- performance(XGboost_pred_testing,"tpr","fpr") #Create ROC curve data
plot(XGboost_ROC_testing) #Plot ROC curve

####AUC
auc.tmp <- performance(XGboost_pred_testing,"auc") #Create AUC data
XGboost_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
XGboost_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(XGboost_prediction, testing$Churn, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

#######NNET##########

my.grid <- expand.grid( .size = c(1,2,4),.decay = c(0.25,1,2)) # Tuning grid for Neural Net

model_NN <- train(Churn ~ gender + SeniorCitizen + Partner + Dependents + PhoneService + MultipleLines
                  + InternetService + OnlineSecurity + OnlineBackup + DeviceProtection + TechSupport
                  + StreamingTV + StreamingMovies + Contract + PaperlessBilling + PaymentMethod
                  + tenure_bin + MonthlyCharges_bin + TotalCharges_bin + monthlychargespercentageoftotal
                  + averagemonthlybill_scaled + tenure_scaled + MonthlyCharges_scaled + TotalCharges_scaled , 
                  data = training, method = "nnet", tuneGrid = my.grid, trace = TRUE, na.action =na.omit)


plot(model_NN) #Visualize the relationship between the number of layers, decay and accuracy

NN_testing <- predict(model_NN, newdata=testing) #Predict classification 
NN_probabilities_testing <-predict(model_NN,newdata=testing,type = "prob")
NN_pred_testing <- prediction(NN_probabilities_testing[,2], testing$Churn) #Calculate errors

confusionMatrix(NN_testing,testing$Churn,positive = "1") # 72% accuracy

NN_classification <- rep("1",2108)
#NN_classification[NN_testing[,2] < 0.26] = "0" 
NN_classification <- as.factor(NN_classification)

NN_testing_results <- cbind(NN_testing, NN_classification)
#write.csv(NN_testing_results, "NN_prediction_prob_results.csv",row.names = TRUE)


#ROC curve and AUC

NN_ROC_testing <- performance(NN_pred_testing,"tpr","fpr") #Create ROC curve data
plot(NN_ROC_testing) #Plot ROC curve

auc.tmp <- performance(NN_pred_testing,"auc") #Create AUC data
NN_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
NN_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

plotLift(NN_testing,  testing$Churn, cumulative = TRUE, n.buckets = 10) # Plot Lift chart
