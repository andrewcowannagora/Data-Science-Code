library(mice)
library(ROSE)
library(MASS)
library(caret)
library(pROC)
library(randomForest)
library(e1071)
library(ggplot2)
library(MLmetrics)
library(OneR)
library(dplyr)
library(corrplot)
library(partykit)

##Kaggle Problem description and data: https://www.kaggle.com/ntnu-testimon/paysim1

project<-read.csv("\\Finance Project\\data.csv", header = TRUE, sep = ",", strip.white=TRUE)
md.pattern(project)
table(project$isFraud)
prop.table(table(project$isFraud))
set.seed(705)

######### feature engineering  #####################
project$amount <- round(project$amount, digits = 0)
project$oldbalanceOrg<- round(project$oldbalanceOrg, digits = 0)
project$oldbalanceDest <- round(project$oldbalanceDest, digits = 0)
project$newbalanceOrig <- round(project$newbalanceOrig, digits = 0)
project$newbalanceDest <- round(project$newbalanceDest, digits = 0)

#convert type to dummy variables
project$cashout <- ifelse(project$type == "CASH_OUT",1,0)
project$transfer <- ifelse(project$type == "TRANSFER",1,0)
#project$payment <- ifelse(project$type == "PAYMENT",1,0)
#project$debit <- ifelse(project$type == "DEBIT",1,0)
#project$cashin <- ifelse(project$type == "CASH_IN",1,0)

#take log transformation to amount
project$amount_log <- log(project$amount+1)

#identify name orig is customer or merchandise, create dummy variable for this, after checking, no M for name orig
#project$orig_c <- ifelse(substr(project$nameOrig,0,1) == "C",1,0)

#identify name destination is customer or merchandise, create dummy variable for this
project$dest_c <- ifelse(substr(project$nameDest,0,1) == "C",1,0)

#take log transformation to old balance origin, new balance origin, old balance destination, new balance destination
project$old_org_log <- log(project$oldbalanceOrg+1)
project$old_des_log <- log(project$oldbalanceDest+1)
project$new_org_log <- log(project$newbalanceOrig+1)
project$new_des_log <- log(project$newbalanceDest+1)

#create buckets for amount
project$amount1 <- ifelse(project$amount <= 20000,1,0)
project$amount2 <- ifelse(project$amount > 20000 & project$amount <= 120000,1,0)
project$amount3 <- ifelse(project$amount > 120000 & project$amount <= 310000,1,0)
project$amount4 <- ifelse(project$amount > 310000,1,0)

#create bins for balance origin, new balance origin, old balance destination, new balance destination
project$old_org1 <- ifelse(project$oldbalanceOrg == 0,1,0)
project$old_org2 <- ifelse(project$oldbalanceOrg > 0 & project$oldbalanceOrg <= 40000,1,0)
project$old_org3 <- ifelse(project$oldbalanceOrg > 40000 & project$oldbalanceOrg <= 500000,1,0)
project$old_org4 <- ifelse(project$oldbalanceOrg > 500000,1,0)

project$old_des1 <- ifelse(project$oldbalanceDest == 0,1,0)
project$old_des2 <- ifelse(project$oldbalanceDest > 0 & project$oldbalanceDest <= 300000,1,0)
project$old_des3 <- ifelse(project$oldbalanceDest > 300000 & project$oldbalanceDest <= 1500000,1,0)
project$old_des4 <- ifelse(project$oldbalanceDest > 1500000,1,0)

project$new_org1 <- ifelse(project$newbalanceOrig == 0,1,0)
project$new_org2 <- ifelse(project$newbalanceOrig > 0 & project$newbalanceOrig <= 100000,1,0)
project$new_org3 <- ifelse(project$newbalanceOrig > 100000 & project$newbalanceOrig <= 500000,1,0)
project$new_org4 <- ifelse(project$newbalanceOrig > 500000,1,0)

project$new_des1 <- ifelse(project$newbalanceDest == 0,1,0)
project$new_des2 <- ifelse(project$newbalanceDest > 0 & project$newbalanceDest <= 400000,1,0)
project$new_des3 <- ifelse(project$newbalanceDest > 400000 & project$newbalanceDest <= 2000000,1,0)
project$new_des4 <- ifelse(project$newbalanceDest > 2000000,1,0)

#calculate the change in initial account, and keep them in whole number
project$change_org <- round(project$oldbalanceOrg-project$newbalanceOrig, digits = 0)
project$change_new <- round(project$newbalanceDest-project$oldbalanceDest, digits = 0)
#create indicator if changes are the same
project$change_same <- ifelse(project$change_org == project$change_new,1,0)
#create indicators if transaction amount equal to account change
project$equal_org <- ifelse(project$amount == abs(project$change_org),1,0)
project$equal_new <- ifelse(project$amount == abs(project$change_new),1,0)

#create features for time, 744 steps, 24h*31 days
project$time0_6 <- ifelse(project$step %in% c(1,2,3,4,5,6,25,26,27,28,29,30,49,50,51,52,53,54,73,74,75,76,77,78,97,98,99,100,101,102,121,122,123,
                                              124,125,126,145,146,147,148,149,150,169,170,171,172,173,174,193,194,195,196,197,198,217,218,219,
                                              220,221,222,241,242,243,244,245,246,265,266,267,268,269,270,289,290,291,292,293,294,313,314,315,316,
                                              317,318,337,338,339,340,341,342,361,362,363,364,365,366,385,386,387,388,389,390,409,410,411,412,413,
                                              414,433,434,435,436,437,438,457,458,459,460,461,462,481,482,483,484,485,486,505,506,507,508,509,510,
                                              529,530,531,532,533,534,553,554,555,556,557,558,577,578,579,580,581,582,601,602,603,604,605,606,625,
                                              626,627,628,629,630,649,650,651,652,653,654,673,674,675,676,677,678,697,698,699,700,701,702,721,722,
                                              723,724,725,726),1,0)

project$time6_12 <- ifelse(project$step %in% c(7,8,9,10,11,12,31,32,33,34,35,36,55,56,57,58,59,60,79,80,81,82,83,84,103,104,105,106,107,108,127,
                                               128,129,130,131,132,151,152,153,154,155,156,175,176,177,178,179,180,199,200,201,202,203,204,223,
                                               224,225,226,227,228,247,248,249,250,251,252,271,272,273,274,275,276,295,296,297,298,299,300,319,320,
                                               321,322,323,324,343,344,345,346,347,348,367,368,369,370,371,372,391,392,393,394,395,396,415,416,
                                               417,418,419,420,439,440,441,442,443,444,463,464,465,466,467,468,487,488,489,490,491,492,511,512,513,
                                               514,515,516,535,536,537,538,539,540,559,560,561,562,563,564,583,584,585,586,587,588,607,608,609,610,
                                               611,612,631,632,633,634,635,636,655,656,657,658,659,660,679,680,681,682,683,684,703,704,705,706,707,
                                               708,727,728,729,730,731,732),1,0)

project$time12_18 <- ifelse(project$step %in% c(13,14,15,16,17,18,37,38,39,40,41,42,61,62,63,64,65,66,85,86,87,88,89,90,109,110,111,112,113,114,
                                                133,134,135,136,137,138,157,158,159,160,161,162,181,182,183,184,185,186,205,206,207,208,209,210,
                                                229,230,231,232,233,234,253,254,255,256,257,258,277,278,279,280,281,282,301,302,303,304,305,306,
                                                325,326,327,328,329,330,349,350,351,352,353,354,373,374,375,376,377,378,397,398,399,400,401,402,
                                                421,422,423,424,425,426,445,446,447,448,449,450,469,470,471,472,473,474,493,494,495,496,497,498,
                                                517,518,519,520,521,522,541,542,543,544,545,546,565,566,567,568,569,570,589,590,591,592,593,594,
                                                613,614,615,616,617,618,637,638,639,640,641,642,661,662,663,664,665,666,685,686,687,688,689,690,
                                                709,710,711,712,713,714,733,734,735,736,737,738),1,0)

project$time18_24 <- ifelse(project$step %in% c(19,20,21,22,23,24,43,44,45,46,47,48,67,68,69,70,71,72,91,92,93,94,95,96,115,116,117,118,119,120,
                                                139,140,141,142,143,144,163,164,165,166,167,168,187,188,189,190,191,192,211,212,213,214,215,216,
                                                235,236,237,238,239,240,259,260,261,262,263,264,283,284,285,286,287,288,307,308,309,310,311,312,
                                                331,332,333,334,335,336,355,356,357,358,359,360,379,380,381,382,383,384,403,404,405,406,407,408,
                                                427,428,429,430,431,432,451,452,453,454,455,456,475,476,477,478,479,480,499,500,501,502,503,504,
                                                523,524,525,526,527,528,547,548,549,550,551,552,571,572,573,574,575,576,595,596,597,598,599,600,
                                                619,620,621,622,623,624,643,644,645,646,647,648,667,668,669,670,671,672,691,692,693,694,695,696,
                                                715,716,717,718,719,720,739,740,741,742,743,744),1,0)

project$Monday <- ifelse(project$step %in% c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,169,170,171,172,173,174,175,176,177,
                                             178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,337,338,339,340,341,342,337,338,339,340,341,342,
                                             343,344,345,346,347,348,349,350,351,352,353,354,355,356,357,358,359,360,505,506,507,508,509,510,
                                             511,512,513,514,515,516,517,518,519,520,521,522,523,524,525,526,527,528,673,674,675,676,677,678,
                                             679,680,681,682,683,684,685,686,687,688,689,690,691,692,693,694,695,696),1,0)

project$Tuesday <- ifelse(project$step %in% c(25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,193,194,195,196,197,198,
                                              199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,361,362,363,364,365,366,
                                              367,368,369,370,371,372,373,374,375,376,377,378,379,380,381,382,383,384,529,530,531,532,533,534,
                                              535,536,537,538,539,540,541,542,543,544,545,546,547,548,549,550,551,552,
                                              697,698,699,700,701,702,703,704,705,706,707,708,709,710,711,712,713,714,715,716,717,718,719,720),1,0)

project$Wednesday <- ifelse(project$step %in% c(49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,217,218,219,
                                                220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,236,237,238,239,240,385,386,387,388,389,390,
                                                391,392,393,394,395,396,397,398,399,400,401,402,403,404,405,406,407,408,553,554,555,556,557,558,
                                                559,560,561,562,563,564,565,566,567,568,569,570,571,572,573,574,575,576,721,722,723,724,725,726,
                                                727,728,729,730,731,732,733,734,735,736,737,738,739,740,741,742,743,744),1,0)

project$Thursday <- ifelse(project$step %in% c(73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,241,242,243,244,245,246,
                                               247,248,249,250,251,252,253,254,255,256,257,258,259,260,261,262,263,264,409,410,411,412,413,414,
                                               415,416,417,418,419,420,421,422,423,424,425,426,427,428,429,430,431,432,
                                               577,578,579,580,581,582,583,584,585,586,587,588,589,590,591,592,593,594,595,596,597,598,599,600),1,0)

project$Friday <- ifelse(project$step %in% c(97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,
                                             265,266,267,268,269,270,271,272,273,274,275,276,277,278,279,280,281,282,283,284,285,286,287,288,
                                             433,434,435,436,437,438,439,440,441,442,443,444,445,446,447,448,449,450,451,452,453,454,455,456,
                                             601,602,603,604,605,606,607,608,609,610,611,612,613,614,615,616,617,618,619,620,621,622,623,624),1,0)

project$Saturday <- ifelse(project$step %in% c(121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,
                                               289,290,291,292,293,294,295,296,297,298,299,300,301,302,303,304,305,306,307,308,309,310,311,312,
                                               457,458,459,460,461,462,463,464,465,466,467,468,469,470,471,472,473,474,475,476,477,478,479,480,
                                               625,626,627,628,629,630,631,632,633,634,635,636,637,638,639,640,641,642,643,644,645,646,647,648),1,0)

project$Sunday <- ifelse(project$step %in% c(145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,
                                             313,314,315,316,317,318,319,320,321,322,323,324,325,326,327,328,329,330,331,332,333,334,335,336,
                                             481,482,483,484,485,486,487,488,489,490,491,492,493,494,495,496,497,498,499,500,501,502,503,504,
                                             649,650,651,652,653,654,655,656,657,658,659,660,661,662,663,664,665,666,667,668,669,670,671,672),1,0)


fraud <- project[which(project$isFraud==1),]
par(mar=c(4,4,4,4))
plot(fraud$type)
small <- project[which(project$type=="CASH_OUT" | project$type=="TRANSFER"),]
table(small$isFraud)

####### up sample #########
n_legit <- 2753983
new_frac_legit <- 0.8
new_n_total <- n_legit/new_frac_legit

oversample <- ovun.sample(isFraud ~ . ,data = small, method = "over", N = new_n_total)
over_data <- oversample$data

some1 <- select(over_data,step,oldbalanceOrg,newbalanceOrig,oldbalanceDest,newbalanceDest)
cor1 <- cor(some1)
corr1 <- corrplot(cor1,method = "color")

hold1 <- sample(seq_len(nrow(over_data)), size = 1000000)
test_over <- over_data[hold1,]
train_over <- over_data[-hold1,]


####### down sample ########
n_legit <- 8213
new_frac_legit <- 0.2
new_n_total <- n_legit/new_frac_legit

downsample <- ovun.sample(isFraud ~ . ,data = small, method = "under", N = new_n_total)
down_data <- downsample$data

some2 <- select(down_data,step,oldbalanceOrg,newbalanceOrig,oldbalanceDest,newbalanceDest)
cor2 <- cor(some2)
corr2 <- corrplot(cor2,method = "color")

hold2 <- sample(seq_len(nrow(down_data)), size = 12000)
test_down <- down_data[hold2,]
train_down <- down_data[-hold2,]

####### original  ##########
some3 <- select(project,step,oldbalanceOrg,newbalanceOrig,oldbalanceDest,newbalanceDest)
cor3 <- cor(some3)
corr3 <- corrplot(cor3,method = "color")

hold3 <- sample(seq_len(nrow(project)), size = 1900000)
test_org <- project[hold3,]
train_org <- project[-hold3,]

###### initial model for up sample #######
glm_up_int <- glm(isFraud~step+type+amount+oldbalanceOrg+newbalanceOrig+oldbalanceDest+newbalanceDest+isFlaggedFraud,data=train_over)
stepwise_up_int <- stepAIC(glm_up_int,direction="both",trace=FALSE)
summary(stepwise_up_int)

#predictions based on stepwise, select 0.6 as cutoff, calculate confusion matrix, F1 score, error rate, AUC
pred_stepwise_up_int <- predict(stepwise_up_int,test_over)
pred_stepwise_up_int1 <- rep("1",1000000)
pred_stepwise_up_int1[pred_stepwise_up_int<mean(pred_stepwise_up_int,0.6)]="0"
pred_stepwise_up_int1 <- as.factor(pred_stepwise_up_int1)
confusionMatrix(pred_stepwise_up_int1,as.factor(test_over$isFraud),positive="1") #acc:0.656 sen:0.8944 spe:0.5973
#Prediction      0      1
########### 0 479091  20909
########### 1 322997 177003
F1_Score(test_over$isFraud, pred_stepwise_up_int1, positive = 1)  #0.5072
error_glm_up_int <- 1- sum(pred_stepwise_up_int1 == test_over$isFraud)/length(pred_stepwise_up_int1) #0.3439
curve_stepwise_up_int <- roc(test_over$isFraud,pred_stepwise_up_int,smooth=F,ci=T)
curve_stepwise_up_int  #0.8804

###### glm model for up sample with feature engineering#######
glm_up <- glm(isFraud~isFlaggedFraud+amount_log+dest_c+old_org_log+old_des_log+new_org_log+new_des_log+amount1+ 
                amount2+amount3+old_org1+old_org2+old_org3+old_des1+old_des2+old_des3+new_org1+new_org2+new_org3+ 
                new_des1+new_des2+new_des3+change_org+change_new+change_same+equal_org+equal_new+time0_6+time6_12+time12_18+Monday+ 
                Tuesday+Wednesday+Friday+Saturday+Sunday,data=train_over)
stepwise_up <- stepAIC(glm_up,direction="both",trace=FALSE)
summary(stepwise_up)

#predictions based on stepwise, select 0.6 as cutoff, calculate confusion matrix, F1 score, error rate, AUC
pred_stepwise_up <- predict(stepwise_up,test_over)
pred_stepwise_up1 <- rep("1",1000000)
pred_stepwise_up1[pred_stepwise_up<mean(pred_stepwise_up,0.6)]="0"
pred_stepwise_up1 <- as.factor(pred_stepwise_up1)
confusionMatrix(pred_stepwise_up1,as.factor(test_over$isFraud),positive="1") #acc:0.6966 sen:0.9968 spe:0.6225
#Prediction      0      1
########### 0 499372    628
########### 1 302781 197219
F1_Score(test_over$isFraud, pred_stepwise_up1, positive = 1)  #0.5652
error_glm_up <- 1- sum(pred_stepwise_up1 == test_over$isFraud)/length(pred_stepwise_up1) #0.3034
curve_stepwise_up <- roc(test_over$isFraud,pred_stepwise_up,smooth=F,ci=T)
curve_stepwise_up  #0.9975

###### initial model for down sample #######
glm_down_int <- glm(isFraud~step+type+amount+oldbalanceOrg+newbalanceOrig+oldbalanceDest+newbalanceDest+isFlaggedFraud,data=train_down)
stepwise_down_int <- stepAIC(glm_down_int,direction="both",trace=FALSE)
summary(stepwise_down_int)

#predictions based on stepwise, select 0.6 as cutoff, calculate confusion matrix, F1 score, error rate, AUC
pred_stepwise_down_int <- predict(stepwise_down_int,test_down)
pred_stepwise_down_int1 <- rep("1",12000)
pred_stepwise_down_int1[pred_stepwise_down_int<mean(pred_stepwise_down_int,0.6)]="0"
pred_stepwise_down_int1 <- as.factor(pred_stepwise_down_int1)
confusionMatrix(pred_stepwise_down_int1,as.factor(test_down$isFraud),positive="1") #acc:0.6547 sen:0.8952 spe:0.5961
#Prediction      0      1
########### 0   5754  246
########### 1   3898 2102
F1_Score(test_down$isFraud, pred_stepwise_down_int1, positive = 1)  #0.5036
error_glm_down_int <- 1- sum(pred_stepwise_down_int1 == test_down$isFraud)/length(pred_stepwise_down_int1) #0.3453
curve_stepwise_down_int <- roc(test_down$isFraud,pred_stepwise_down_int,smooth=F,ci=T)
curve_stepwise_down_int  #0.8756

###### glm model for down sample with feature engineering#######
glm_down <- glm(isFraud~isFlaggedFraud+cashout+transfer+amount_log+dest_c+old_org_log+old_des_log+new_org_log+new_des_log+amount1+ 
                  amount2+amount3+amount4+old_org1+old_org2+old_org3+old_org4+old_des1+old_des2+old_des3+old_des4+new_org1+new_org2+new_org3+new_org4+ 
                  new_des1+new_des2+new_des3+new_des4+change_org+change_new+change_same+equal_org+equal_new+time0_6+time6_12+time12_18+time18_24+Monday+ 
                  Tuesday+Wednesday+Thursday+Friday+Saturday+Sunday,data=train_down)
stepwise_down <- stepAIC(glm_down,direction="both",trace=FALSE)
summary(stepwise_down)

#predictions based on stepwise, select 0.6 as cutoff, calculate confusion matrix, F1 score, error rate, AUC
pred_stepwise_down <- predict(stepwise_down,test_down)
pred_stepwise_down1 <- rep("1",12000)
pred_stepwise_down1[pred_stepwise_down<mean(pred_stepwise_down,0.6)]="0"
pred_stepwise_down1 <- as.factor(pred_stepwise_down1)
confusionMatrix(pred_stepwise_down1,as.factor(test_down$isFraud),positive="1") #acc:0.7012 sen:0.9979 spe:0.626
#Prediction      0      1
########### 0   5995    5
########### 1   3581 2419
F1_Score(test_down$isFraud, pred_stepwise_down1, positive = 1)  #0.5613
error_glm_down <- 1- sum(pred_stepwise_down1 == test_down$isFraud)/length(pred_stepwise_down1) #0.3052
curve_stepwise_down <- roc(test_down$isFraud,pred_stepwise_down,smooth=F,ci=T)
curve_stepwise_down  #0.9984

###### initial model for original data #######
glm_org_int <- glm(isFraud~step+type+amount+oldbalanceOrg+newbalanceOrig+oldbalanceDest+newbalanceDest+isFlaggedFraud,data=train_org)
stepwise_org_int <- stepAIC(glm_org_int,direction="both",trace=FALSE)
summary(stepwise_org_int)

#predictions based on stepwise, select 0.6 as cutoff, calculate confusion matrix, F1 score, error rate, AUC
pred_stepwise_org_int <- predict(stepwise_org_int,test_org)
pred_stepwise_org_int1 <- rep("1",1900000)
pred_stepwise_org_int1[pred_stepwise_org_int<mean(pred_stepwise_org_int,0.6)]="0"
pred_stepwise_org_int1 <- as.factor(pred_stepwise_org_int1)
confusionMatrix(pred_stepwise_org_int1,as.factor(test_org$isFraud),positive="1") #acc:0.5013 sen:0.9807 spe:0.5006
#Prediction      0      1
########### 0 949952     48
########### 1 947557   2443
F1_Score(test_org$isFraud, pred_stepwise_org_int1, positive = 1)  #0.0051
error_glm_org_int <- 1- sum(pred_stepwise_org_int1 == test_org$isFraud)/length(pred_stepwise_org_int1) #0.4987
curve_stepwise_org_int <- roc(test_org$isFraud,pred_stepwise_org_int,smooth=F,ci=T)
curve_stepwise_org_int  #0.9454

###### glm model for original data with feature engineering#######
glm_org <- glm(isFraud~isFlaggedFraud+cashout+transfer+amount_log+dest_c+old_org_log+old_des_log+new_org_log+new_des_log+amount1+ 
                 amount2+amount3+amount4+old_org1+old_org2+old_org3+old_org4+old_des1+old_des2+old_des3+new_org1+new_org2+new_org3+new_org4+ 
                 new_des1+new_des2+new_des3+change_org+change_new+change_same+equal_org+equal_new+time0_6+time6_12+time12_18+Monday+ 
                 Tuesday+Wednesday+Thursday+Friday+Saturday+Sunday,data=train_org)
stepwise_org <- stepAIC(glm_org,direction="both",trace=FALSE)
summary(stepwise_org)

#predictions based on stepwise, select 0.6 as cutoff, calculate confusion matrix, F1 score, error rate, AUC
pred_stepwise_org <- predict(stepwise_org,test_org)
pred_stepwise_org1 <- rep("1",1900000)
pred_stepwise_org1[pred_stepwise_org<mean(pred_stepwise_org,0.6)]="0"
pred_stepwise_org1 <- as.factor(pred_stepwise_org1)
confusionMatrix(pred_stepwise_org1,as.factor(test_org$isFraud),positive="1") #acc:0.5013 sen:0.9971 spe:0.5006
#Prediction      0      1
########### 0 949993      7
########### 1 947611   2389
F1_Score(test_org$isFraud, pred_stepwise_org1, positive = 1)  #0.005
error_glm_org <- 1- sum(pred_stepwise_org1 == test_org$isFraud)/length(pred_stepwise_org1) #0.4987
curve_stepwise_org <- roc(test_org$isFraud,pred_stepwise_org,smooth=F,ci=T)
curve_stepwise_org  #0.9949

####### Random Forest for down sample ##############
train_down$isFraud <- as.factor(train_down$isFraud)
test_down$isFraud <- as.factor(test_down$isFraud)
rf_down <- randomForest(isFraud~isFlaggedFraud+cashout+transfer+amount_log+dest_c+old_org_log+old_des_log+new_org_log+new_des_log+amount1+ 
                          amount2+amount3+amount4+old_org1+old_org2+old_org3+old_org4+old_des1+old_des2+old_des3+old_des4+new_org1+new_org2+new_org3+new_org4+ 
                          new_des1+new_des2+new_des3+new_des4+change_org+change_new+change_same+equal_org+equal_new+time0_6+time6_12+time12_18+time18_24+Monday+ 
                          Tuesday+Wednesday+Thursday+Friday+Saturday+Sunday,data=train_down,
                        importance=TRUE,proximity=FALSE,cutoff=c(0.6,0.4),type="classification")

pred_rf_down <- predict(rf_down,test_down)
confusionMatrix(pred_rf_down,test_down$isFraud,positive="1") #acc:0.9989 sen:0.9966 spe:0.9995
#Prediction      0      1
########### 0   9647    8
########### 1      5 2340
F1_Score(test_down$isFraud, pred_rf_down, positive = 1)  #0.9972
error_rf_down <- 1- sum(pred_rf_down == test_down$isFraud)/length(pred_rf_down) #0.0011
curve_rf_down <- roc(test_down$isFraud,as.numeric(pred_rf_down),smooth=F,ci=T)
curve_rf_down #0.998

varImpPlot(rf_down)

####### SVM for down sample ##############
train_down$isFraud <- as.factor(train_down$isFraud)
test_down$isFraud <- as.factor(test_down$isFraud)
train_fraud <- subset(train_down, select = c(isFraud))
train_down_no <- subset(train_down, select = -c(type,nameOrig,nameDest,isFraud,dest_c))
train_down_s <- scale(train_down_no,center = TRUE, scale = TRUE)
train_down_s <- cbind(train_fraud, train_down_s)
test_fraud <- subset(test_down, select = c(isFraud))
test_down_no <- subset(test_down, select = -c(type,nameOrig,nameDest,isFraud,dest_c))
test_down_s <- scale(test_down_no,center = TRUE, scale = TRUE)
test_down_s <- cbind(test_fraud, test_down_s)
svm_down <- svm(isFraud~isFlaggedFraud+cashout+transfer+amount_log+old_org_log+old_des_log+new_org_log+new_des_log+amount1+ 
                           amount2+amount3+amount4+old_org1+old_org2+old_org3+old_org4+old_des1+old_des2+old_des3+old_des4+new_org1+new_org2+new_org3+new_org4+ 
                           new_des1+new_des2+new_des3+new_des4+change_org+change_new+change_same+equal_org+equal_new+time0_6+time6_12+time12_18+time18_24+Monday+ 
                           Tuesday+Wednesday+Thursday+Friday+Saturday+Sunday,data=train_down,
                         type = 'C-classification', kernel = 'linear', cost=0.1)

pred_svm_down <- predict(svm_down,test_down_s)
confusionMatrix(pred_svm_down,as.factor(test_down_s$isFraud),positive="1") #acc:0.9959 sen:0.9798 spe:1
#Prediction      0      1
########### 0   9576   49
########### 1      0 2375  
F1_Score(test_down$isFraud, pred_svm_down, positive = 1)  #0.9898
error_svm_down <- 1- sum(pred_svm_down == test_down$isFraud)/length(pred_svm_down) #0.0041
curve_svm_down <- roc(test_down$isFraud,as.numeric(pred_svm_down),smooth=F,ci=T)
curve_svm_down #0.9899

###### ctree model for up sample with feature engineering#######
train_over$isFraud <- as.factor(train_over$isFraud)
test_over$isFraud <- as.factor(test_over$isFraud)
ctree_up <- ctree(isFraud~isFlaggedFraud+amount_log+dest_c+old_org_log+old_des_log+new_org_log+new_des_log+amount1+ 
                    amount2+amount3+old_org1+old_org2+old_org3+old_des1+old_des2+old_des3+new_org1+new_org2+new_org3+ 
                    new_des1+new_des2+new_des3+change_org+change_new+change_same+equal_org+equal_new+time0_6+time6_12+time12_18+Monday+ 
                    Tuesday+Wednesday+Friday+Saturday+Sunday,data=train_over)

#predictions based on ctree, select 0.6 as cutoff, calculate confusion matrix, F1 score, error rate, AUC
pred_ctree_up <- predict(ctree_up,test_over)
confusionMatrix(pred_ctree_up,as.factor(test_over$isFraud),positive="1") #acc:0.0.9989 sen:0.9946 spe:1
#Prediction      0      1
########### 0 802083   1078
########### 1      5 196834
F1_Score(test_over$isFraud, pred_ctree_up, positive = 1)  #0.9973
error_ctree_up <- 1- sum(pred_ctree_up == test_over$isFraud)/length(pred_ctree_up1) #0.0011
curve_ctree_up <- roc(test_over$isFraud,as.numeric(pred_ctree_up),smooth=F,ci=T)
curve_ctree_up  #0.9973

###### ctree model for down sample with feature engineering#######
ctree_down <- ctree(isFraud~isFlaggedFraud+cashout+transfer+amount_log+dest_c+old_org_log+old_des_log+new_org_log+new_des_log+amount1+ 
                      amount2+amount3+amount4+old_org1+old_org2+old_org3+old_org4+old_des1+old_des2+old_des3+old_des4+new_org1+new_org2+new_org3+new_org4+ 
                      new_des1+new_des2+new_des3+new_des4+change_org+change_new+change_same+equal_org+equal_new+time0_6+time6_12+time12_18+time18_24+Monday+ 
                      Tuesday+Wednesday+Thursday+Friday+Saturday+Sunday,data=train_down)
plot(ctree_down, gp = gpar(fontsize = 8))
summary(ctree_down)

#predictions based on ctree, select 0.6 as cutoff, calculate confusion matrix, F1 score, error rate, AUC
pred_ctree_down <- predict(ctree_down,test_down)
confusionMatrix(pred_ctree_down,as.factor(test_down$isFraud),positive="1") #acc:0.999 sen:0.995 spe:1
#Prediction      0      1
########### 0 9576     12
########### 1    0   2412
F1_Score(test_down$isFraud, pred_ctree_down, positive = 1)  #0.9975
error_ctree_down <- 1- sum(pred_ctree_down == test_down$isFraud)/length(pred_ctree_down) #0.001
curve_ctree_down <- roc(test_down$isFraud,as.numeric(pred_ctree_down),smooth=F,ci=T)
curve_ctree_down  #0.9975

###### ctree model for original data with feature engineering#######
train_org$isFraud <- as.factor(train_org$isFraud)
test_org$isFraud <- as.factor(test_org$isFraud)
ctree_org <- ctree(isFraud~isFlaggedFraud+cashout+transfer+amount_log+dest_c+old_org_log+old_des_log+new_org_log+new_des_log+amount1+ 
                     amount2+amount3+amount4+old_org1+old_org2+old_org3+old_org4+old_des1+old_des2+old_des3+new_org1+new_org2+new_org3+new_org4+ 
                     new_des1+new_des2+new_des3+change_org+change_new+change_same+equal_org+equal_new+time0_6+time6_12+time12_18+Monday+ 
                     Tuesday+Wednesday+Thursday+Friday+Saturday+Sunday,data=train_org)

#predictions based on ctree, select 0.6 as cutoff, calculate confusion matrix, F1 score, error rate, AUC
pred_ctree_org <- predict(ctree_org,test_org)
confusionMatrix(pred_ctree_org,as.factor(test_org$isFraud),positive="1") #acc:0.9995 sen:0.6411 spe:0.9999
#Prediction      0      1
########### 0 1897592     860
########### 1      12    1536
F1_Score(test_org$isFraud, pred_ctree_org, positive = 1)  #0.7789
error_ctree_org <- 1- sum(pred_ctree_org == test_org$isFraud)/length(pred_ctree_org) #0.0005
curve_ctree_org <- roc(test_org$isFraud,as.numeric(pred_ctree_org),smooth=F,ci=T)
curve_ctree_org  #0.8205

