
if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not

pacman::p_load("nnet","caret", "lift") #Check, and if needed install the necessary packages


#STC(A).csv
STCdata_A<-read.csv(file.choose(), na.strings=c(""," ","NA"), header=TRUE) # Load the datafile to R

# Load the data, correct mis-classified datafields, fixNAs -- same as you did in the logistic regression file
# To ensure "appled-to-apples" comparisons with logistic regression, use the same training and testing -- the code below only works in the same R session after you've ran the logistic regression code

##start Neural Network analysis

my.grid <- expand.grid( .size = c(1,2,4),.decay = c(0.25,1,2)) # Tuning grid for Neural Net

model_NN <- train(Retained.in.2012.~ Special.Pay + 
                    To.Grade + Group.State + Is.Non.Annual. +
                    Tuition + FRP.Active + FRP.Cancelled + FRP.Take.up.percent. + 
                    Cancelled.Pax + Total.Discount.Pax + Initial.System.Date + 
                    Poverty.Code + CRM.Segment + School.Type + Parent.Meeting.Flag + 
                    MDR.Low.Grade + MDR.High.Grade + Total.School.Enrollment + 
                    EZ.Pay.Take.Up.Rate + School.Sponsor +  
                    SPR.New.Existing + FPP + FirstMeeting + LastMeeting + 
                    DifferenceTraveltoFirstMeeting + DepartureMonth  + MajorProgramCode + SingleGradeTripFlag + 
                    FPP.to.School.enrollment + FPP.to.PAX + SchoolSizeIndicator, 
                    data = training, method = "nnet", tuneGrid = my.grid, trace = TRUE, na.action =na.omit)

plot(model_NN) #Visualize the relationship between the number of layers, decay and accuracy

NN_prediction<-predict(model_NN, newdata=testing) #Predict classification 
confusionMatrix(NN_prediction,testing$Retained.in.2012.,positive = "1") # 72% accuracy

#ROC curve and AUC

NN_probabilities_testing <-predict(model_NN,newdata=testing,type = "prob") #Predict probabilities
NN_pred_testing <- prediction(NN_probabilities_testing[,2], testing$Retained.in.2012.) #Calculate errors
NN_ROC_testing <- performance(NN_pred_testing,"tpr","fpr") #Create ROC curve data
plot(NN_ROC_testing) #Plot ROC curve

auc.tmp <- performance(NN_pred_testing,"auc") #Create AUC data
NN_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
NN_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

plotLift(NN_prediction,  testing$Retained.in.2012., cumulative = TRUE, n.buckets = 10) # Plot Lift chart
