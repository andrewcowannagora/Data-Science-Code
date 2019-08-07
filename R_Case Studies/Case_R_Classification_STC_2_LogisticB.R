### This code is to be ran after running the (A) case code in the same R session


#STC(A).csv
STCdata_A<-read.csv(file.choose(), na.strings=c(""," ","NA"), header=TRUE) # Load the datafile to R

#STC(B).csv
STCdata_B<-read.csv(file.choose()) # Load the (B) case datafile to R

str(STCdata_B) # See if some data types were misclassified when importing data from CSV

STCdata_merged = merge(STCdata_A, STCdata_B, by= 'ID') # merge the data

#Apply the fixNAs and combine categories functions to the merged data and then split it into testing and training data.
STCdata_merged<-fixNAs(STCdata_merged)
STCdata_merged<-combinerarecategories(STCdata_merged, 10)

training <- STCdata_merged[ inTrain,]
testing <- STCdata_merged[ -inTrain,]

#Add the new NPS variables to the regression formula
model_logistic_B<-glm(Retained.in.2012.~ Special.Pay + 
                      To.Grade + Group.State + Is.Non.Annual. +
                      Tuition + FRP.Active + FRP.Cancelled + FRP.Take.up.percent. + 
                      Cancelled.Pax + Total.Discount.Pax + Initial.System.Date + 
                      Poverty.Code + CRM.Segment + School.Type + Parent.Meeting.Flag + 
                      MDR.Low.Grade + MDR.High.Grade + Total.School.Enrollment + 
                      EZ.Pay.Take.Up.Rate + School.Sponsor +  
                      SPR.New.Existing + FPP + FirstMeeting + LastMeeting + 
                      DifferenceTraveltoFirstMeeting + DepartureMonth  + MajorProgramCode + SingleGradeTripFlag + 
                      FPP.to.School.enrollment + FPP.to.PAX + SchoolSizeIndicator
                    + NPS.2011 + NPS.2010 + NPS.2009 + NPS.2008, data=training, family="binomial"(link="logit"))

model_logistic_stepwiseAIC_B<-stepAIC(model_logistic_B, direction = c("both"),trace = 1) #AIC stepwise
summary(model_logistic_stepwiseAIC_B) 

par(mfrow=c(1,4))
plot(model_logistic_stepwiseAIC_B) #Error plots: similar nature to lm plots
par(mfrow=c(1,1))

###Finding predicitons: probabilities and classification
logistic_probabilities_B<-predict(model_logistic_stepwiseAIC_B,newdata=testing,type="response") #Predict probabilities
logistic_classification_B<-rep("1",500)
logistic_classification_B[logistic_probabilities_B<0.6073]="0" #Predict classification using 0.6073 threshold. Why 0.6073 - that's the average probability of being retained in the data. An alternative code: logistic_classification <- as.integer(logistic_probabilities > mean(testing$Retained.in.2012. == "1"))
logistic_classification_B<-as.factor(logistic_classification_B)

###Confusion matrix  
confusionMatrix(logistic_classification_B,testing$Retained.in.2012.,positive="1") #Display confusion matrix

####ROC Curve
logistic_ROC_prediction_B <- prediction(logistic_probabilities_B, testing$Retained.in.2012.)
logistic_ROC_B <- performance(logistic_ROC_prediction_B,"tpr","fpr") #Create ROC curve data
plot(logistic_ROC_B) #Plot ROC curve

plot(logistic_ROC, add=TRUE, col="red") #For comparison, overlay/add the ROC curve from (A) in red
legend("right", legend=c("(B)", "(A)"), col=c("black", "red"), lty=1:2, cex=0.8)

####AUC (area under curve)
auc.tmp_B <- performance(logistic_ROC_prediction_B,"auc") #Create AUC data
logistic_auc_testing_B <- as.numeric(auc.tmp_B@y.values) #Calculate AUC
logistic_auc_testing_B #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart

plotLift(logistic_probabilities_B, testing$Retained.in.2012., cumulative = TRUE, n.buckets = 10) # Plot Lift chart

### An alternative way is to plot a Lift curve not by buckets, but on all data points
Lift_B <- performance(logistic_ROC_prediction_B,"lift","rpp")
plot(Lift_B)

Lift_A <- performance(logistic_ROC_prediction,"lift","rpp") #this is from the (A) case
plot(Lift_A, add=TRUE, col="red") #For comparison, overlay/add the Lift curve from (A) in red
legend("right", legend=c("(B)", "(A)"), col=c("black", "red"), lty=1:2, cex=0.8)
