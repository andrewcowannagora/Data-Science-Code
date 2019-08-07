#R Classification - Survey Completion Prediction
#Data is too large to upload to Github and is available on request
#See Tableau Public profile for project report



library(openxlsx)
library(sqldf)
library(gbm)
library(pROC)
library(e1071)
library(Metrics)
library(dummies)
library(glmnet)
library(caret)
library(e1071)
library(MASS)

sent <- read.csv("B:\\modeling\\Data Science Files\\data\\sent_service.csv", header = TRUE, sep = ",")
sent_s <- read.csv("B:\\modeling\\Data Science Files\\data\\sent_sale.csv", header = TRUE, sep = ",")
days <- read.csv("B:\\modeling\\Data Science Files\\data\\days_service.csv", header = TRUE, sep = ",")
days_s <- read.csv("B:\\modeling\\Data Science Files\\data\\days_sale.csv", header = TRUE, sep = ",")
optout <- read.csv("B:\\modeling\\Data Science Files\\data\\optout_service.csv", header = TRUE, sep = ",")
optout_s <- read.csv("B:\\modeling\\Data Science Files\\data\\optout_sale.csv", header = TRUE, sep = ",")

w1 <- dummy(sent$weekday1,sep='_')
w2 <- dummy(sent$weekday2,sep='_')
sent <- cbind(sent, dummy(sent$SurveyType,sep='_'), dummy(sent$Brand,sep='_'), dummy(sent$RegionID,sep='_'), dummy(sent$city,sep='_'), 
              dummy(sent$rec_month,sep='_'), dummy(sent$rec_year,sep='_'),dummy(sent$dealer_name,sep='_'),dummy(sent$province,sep='_'),w1,w2)
#names <- c('Metro','customer_only','warranty','customer','campaign','purchase','rec_month','rec_year','odo_19','odo_19_34','odo_34_52',
#           'odo_52_79','odo_79','cust_0','cust_0_130','cust_130_350','cust_350','war_0_150','war_150_290','war_290')
#names <- c('rec_month','rec_year')
#sent[,names] <- lapply(sent[,names],factor)
str(sent)

set.seed(9575)
test_ind <- sample(seq_len(nrow(sent)),size=45000)
test <- sent[test_ind,]
train <- sent[-test_ind,]

gbm_sent <- gbm(response~
                  Metro+customer_only+warranty+customer+campaign+service_days+odo_19+odo_19_34+odo_34_52+odo_52_79+odo_79+cust_0+cust_0_130+
                  cust_130_350+cust_350+war_150+war_150_290+war_290+sent_CustomerPay+sent_Warranty+sent_BMW+sent_BMWi+sent_Central+
                  sent_Eastern+sent_Western+sent_1+sent_2+sent_3+sent_4+sent_5+sent_6+sent_7+sent_8+sent_9+sent_10+sent_11+sent_12+sent_2018+sent_2019+
                  sent_ajax+sent_barrie+sent_blainville+sent_brampton+sent_brossard+
                  sent_calgary+sent_dieppe+sent_dorval+sent_edmonton+sent_halifax+sent_hamilton+sent_kelowna+sent_kingston+sent_kitchener+sent_langley+sent_laval+
                  sent_levis+sent_london+sent_markham+sent_mississauga+sent_montreal+sent_nanaimo+sent_newmarket+sent_northvancouv+sent_northyork+sent_oakville+
                  sent_ottawa+sent_quebec+sent_regina+sent_richmond+sent_richmondhill+sent_rockforest+sent_saintejulie+sent_sarnia+sent_saskatoon+sent_sherbrooke+
                  sent_stcatharine+sent_stcatharines+sent_stjohns+sent_sudbury+sent_terrebonne+sent_thornhill+sent_toronto+sent_troisriviere+sent_vancouver+
                  sent_victoria+sent_winnipeg+sent_woodbridge+sent_AutoWestBMW+sent_BavariaBMW+sent_BemaAutosport+sent_BirchwoodBMW+sent_BMWAutohaus+sent_BMWGallery+
                  sent_BMWGrandRiver+sent_BMWKingston+sent_BMWLangley+sent_BMWLaval+sent_BMWLevis+sent_BMWLondon+sent_BMWMarkham+sent_BMWMoncton+sent_BMWNanaimo+sent_BMWNewmarket+
                  sent_BMWRegina+sent_BMWRichmondHillService+sent_BMWSainteJulie+sent_BMWSarnia+sent_BMWSherbrooke+sent_BMWStJohns+sent_BMWToronto+sent_BMWVictoria+
                  sent_BMWVilleDeQuebec+sent_BMWWestIsland+sent_BrianJesselBMW+sent_BrianJesselBMWPreOwne+sent_BuddsBMW+sent_BuddsBMWHamilton+sent_CalgaryBMW+
                  sent_CanbecBMW+sent_EdmontonBMW+sent_EliteBMW+sent_EndrasBMW+sent_GeorgianBMW+sent_GrenierBMW+sent_HamelAutosDeBlainville+sent_HamelBMW+
                  sent_KelownaBMW+sent_MaranelloBMW+sent_ORegansBMW+sent_Ottos+sent_PalladinoBMW+sent_ParkAvenueBMW+sent_ParkShoreBMW+sent_ParkviewBMW+
                  sent_PerformanceBMW+sent_PerformanceCars+sent_PfaffBMW+sent_PolicaroBMW+sent_SarniaFineCars+sent_TheBMWStore+sent_TroisRivieresBMW+sent_AB+sent_BC+
                  sent_MB+sent_NB+sent_NL+sent_NS+sent_ON+sent_QC+sent_SK+weekday1_1+weekday1_2+weekday1_3+weekday1_4+weekday1_5+weekday1_6+weekday1_7+weekday2_2+weekday2_3+ 
                  weekday2_4+weekday2_5+weekday2_6+weekday2_7+weekday2_NA,
                data=train,
                distribution = "bernoulli",
                n.trees = 60,
                shrinkage = 0.03,
                interaction.depth=8,
                train.fraction = 0.7,
                n.minobsinnode = 600,
                keep.data=TRUE,
                verbose=TRUE,
                n.cores=16)

best.iter <- gbm.perf(gbm_sent,method="test")
print(best.iter)
summary(gbm_sent, n.trees = best.iter)

pred_gbm<-predict(gbm_sent, test, best.iter, type = "response")

confusion_gbm=ifelse(pred_gbm>mean(pred_gbm,0.5),1,0)
table(test$response, confusion_gbm)
curve_gbm=roc(test$response,pred_gbm,smooth=F,ci=T)
curve_gbm

glm_sent <- glm(response~
                  Metro+customer_only+warranty+customer+campaign+service_days+odo_19+odo_19_34+odo_34_52+odo_52_79+cust_0+cust_0_130+
                  cust_130_350+war_150+war_150_290+war_290+sent_CustomerPay+sent_BMW+sent_Central+
                  sent_Eastern+sent_1+sent_2+sent_3+sent_4+sent_5+sent_6+sent_7+sent_8+sent_9+sent_10+sent_11+sent_2018+
                  sent_ajax+sent_barrie+sent_blainville+sent_brampton+sent_brossard+weekday1+weekday2+
                  sent_calgary+sent_dieppe+sent_dorval+sent_edmonton+sent_halifax+sent_hamilton+sent_kelowna+sent_kingston+sent_kitchener+sent_langley+sent_laval+
                  sent_levis+sent_london+sent_markham+sent_mississauga+sent_montreal+sent_nanaimo+sent_newmarket+sent_northvancouv+sent_northyork+sent_oakville+
                  sent_ottawa+sent_quebec+sent_regina+sent_richmond+sent_richmondhill+sent_rockforest+sent_saintejulie+sent_sarnia+sent_saskatoon+sent_sherbrooke+
                  sent_stcatharines+sent_stjohns+sent_sudbury+sent_terrebonne+sent_thornhill+sent_toronto+sent_troisriviere+
                  sent_victoria+sent_BavariaBMW+sent_BMWGallery+sent_BMWSarnia+sent_BrianJesselBMW+sent_BrianJesselBMWPreOwne+sent_EliteBMW+sent_HamelAutosDeBlainville+
                  +weekday1_1+weekday1_2+weekday1_3+weekday1_4+weekday1_5+weekday2_2+weekday2_3+weekday2_4+weekday2_5+weekday2_0,
                data=train,
                family = "binomial"(link="logit"))
summary(glm_sent)
glm_sent_stepwise <-stepAIC(glm_sent,direction=c("both"),trace=1)
summary(glm_sent_stepwise)
glm_sent <-glm(formula = response ~ warranty + campaign + service_days + odo_19 + odo_19_34 + 
                 odo_34_52 + odo_52_79 + cust_0 + cust_130_350 + war_150 + 
                 war_150_290 + war_290 + sent_CustomerPay + sent_BMW + sent_Central + 
                 sent_Eastern + sent_1 + sent_2 + sent_3 + sent_4 + sent_5 + 
                 sent_6 + sent_7 + sent_8 + sent_9 + sent_10 + sent_11 + sent_2018 + 
                 sent_barrie + sent_blainville + sent_brampton + sent_brossard + 
                 weekday1 + weekday2 + sent_calgary + sent_dieppe + sent_dorval + 
                 sent_edmonton + sent_halifax + sent_kingston + sent_langley + 
                 sent_laval + sent_levis + sent_london + sent_markham + sent_montreal + 
                 sent_nanaimo + sent_newmarket + sent_northvancouv + sent_oakville + 
                 sent_ottawa + sent_quebec + sent_regina + sent_richmond + 
                 sent_rockforest + sent_saintejulie + sent_sarnia + sent_saskatoon + 
                 sent_sherbrooke + sent_stcatharines + sent_stjohns + sent_sudbury + 
                 sent_terrebonne + sent_thornhill + sent_toronto + sent_troisriviere + 
                 sent_BavariaBMW + sent_BMWGallery + sent_BrianJesselBMW + 
                 sent_BrianJesselBMWPreOwne + sent_EliteBMW + weekday1_2 + 
                 weekday1_3 + weekday2_2 + weekday2_3 + weekday2_4 + weekday2_5 + 
                 weekday2_0, family = binomial(link = "logit"), data = train)
pred_glm<-predict(glm_sent_stepwise, test)
pred_glm<-predict(glm_sent, test)
pred_glm1<-rep("1",45000)
pred_glm1[pred_glm<mean(pred_glm,0.6)]="0" 
pred_glm1<-as.factor(pred_glm1)
confusionMatrix(pred_glm1,as.factor(test$response),positive = "1")

curve_glm=roc(test$response,pred_glm,smooth=F,ci=T)
curve_glm

#############  sent sales  ######################
w1 <- dummy(sent_s$weekday1,sep='_')
w2 <- dummy(sent_s$weekday2,sep='_')
w3 <- dummy(sent_s$weekday3,sep='_')
term <- dummy(sent_s$current_term,sep='_')
sent_s <- cbind(sent_s,dummy(sent_s$Brand,sep='_'), dummy(sent_s$RegionID,sep='_'), dummy(sent_s$purchase_type,sep='_'), dummy(sent_s$new_cde,sep='_'),
              dummy(sent_s$rec_month,sep='_'), dummy(sent_s$rec_year,sep='_'),dummy(sent_s$dealer_name,sep='_'),term,w1,w2,w3)
test_ind_ss <- sample(seq_len(nrow(sent_s)),size=4250)
test_ss <- sent_s[test_ind_ss,]
train_ss <- sent_s[-test_ind_ss,]

gbm_sent_s <- gbm(response~
                  Metro+current_term+price_0+price_49+price_49_62+price_62_82+price_82+sent_s_BMW+sent_s_BMWi+
                  sent_s_Central+sent_s_Eastern+sent_s_Western+sent_s_Cash+sent_s_Leas+sent_s_Loan+sent_s_N+sent_s_U+sent_s_1+ 
                  sent_s_2+sent_s_3+sent_s_4+sent_s_5+sent_s_6+sent_s_7+sent_s_8+sent_s_9+sent_s_10+sent_s_11+sent_s_12+sent_s_2018+sent_s_2019+
                  current_term_0+current_term_1+current_term_2+current_term_3+current_term_4+current_term_5+current_term_6+current_term_7+current_term_8+
                    sent_s_BemaAutosport+sent_s_BirchwoodBMW+sent_s_BMWAutohaus+sent_s_BMWGallery+sent_s_BMWGrandRiver+sent_s_BMWiAutohaus+sent_s_BMWiLangley+
                    sent_s_AutoWestBMW+sent_s_BavariaBMW+sent_s_BMWiLaval+sent_s_BMWiLevis+sent_s_BMWiMarkham+sent_s_BMWiSainteJulie+sent_s_BMWiSherbrooke+
                    sent_s_BMWiToronto+sent_s_BMWiWestIsland+sent_s_BMWKingston+sent_s_BMWLangley+sent_s_BMWLaval+sent_s_BMWLevis+sent_s_BMWLondon+sent_s_BMWMarkham+
                    sent_s_BMWMoncton+sent_s_BMWNanaimo+sent_s_BMWNewmarket+sent_s_BMWRegina+sent_s_BMWSainteJulie+sent_s_BMWSarnia+sent_s_BMWSherbrooke+sent_s_BMWStJohns+
                    sent_s_BMWToronto+sent_s_BMWVictoria+sent_s_BMWVilleDeQuebec+sent_s_BMWWestIsland+sent_s_BrianJesselBMW+sent_s_BrianJesselBMWPreOw+sent_s_BuddsBMW+
                    sent_s_BuddsBMWHamilton+sent_s_CalgaryBMW+sent_s_CanbecBMW+sent_s_DilawriBMW+sent_s_EdmontonBMW+sent_s_EliteBMW+sent_s_EndrasBMW+sent_s_GeorgianBMW+
                    sent_s_GrenierBMW+sent_s_HamelBMW+sent_s_KelownaBMW+sent_s_MaranelloBMW+sent_s_ORegansBMW+sent_s_Ottos+sent_s_OverseasMotors+sent_s_PalladinoBMW+
                    sent_s_ParkAvenueBMW+sent_s_ParkShoreBMW+sent_s_ParkviewBMW+sent_s_PerformanceBMW+sent_s_PfaffBMW+sent_s_PolicaroBMW+sent_s_TheBMWStore+sent_s_TroisRivieresBMW+
                  weekday1_1+weekday1_2+weekday1_3+weekday1_4+weekday1_5+
                  weekday1_6+weekday1_7+weekday2_0+weekday2_1+weekday2_2+weekday2_3+weekday2_4+weekday2_5+weekday2_6+weekday2_7+weekday3_1+weekday3_2+
                  weekday3_3+weekday3_4+weekday3_5+weekday3_6+weekday3_7,
                data=train_ss,
                distribution = "bernoulli",
                n.trees = 60,
                shrinkage = 0.03,
                interaction.depth=6,
                train.fraction = 0.6,
                n.minobsinnode = 400,
                keep.data=TRUE,
                verbose=TRUE,
                n.cores=16)
best.iter_ss <- gbm.perf(gbm_sent_s,method="test")
print(best.iter_ss)
summary(gbm_sent_s, n.trees = best.iter_ss)

pred_gbm_s<-predict(gbm_sent_s, test_ss, best.iter_ss, type = "response")

output_s <- cbind(test_ss, pred_gbm_s)

confusion_gbm_s=ifelse(output_s$pred_gbm_s>mean(output_s$pred_gbm_s,0.5),1,0)
table(output_s$response, confusion_gbm_s)
curve_gbm_s=roc(output_s$response,output_s$pred_gbm_s,smooth=F,ci=T)
curve_gbm_s

glm_sent_s <- glm(response~
                    Metro+price_0+price_49+price_49_62+price_62_82+sent_s_BMW+sent_s_Central+sent_s_Eastern+sent_s_Cash+sent_s_Leas+sent_s_N+sent_s_1+
                    sent_s_2+sent_s_3+sent_s_4+sent_s_5+sent_s_6+sent_s_7+sent_s_8+sent_s_9+sent_s_10+sent_s_11+sent_s_2018+
                    current_term_1+current_term_2+current_term_3+current_term_4+current_term_5+current_term_6+current_term_7+
                    sent_s_BemaAutosport+sent_s_BirchwoodBMW+sent_s_BMWAutohaus+sent_s_BMWGallery+sent_s_BMWGrandRiver+sent_s_BMWiAutohaus+sent_s_BMWiLangley+
                    sent_s_AutoWestBMW+sent_s_BavariaBMW+sent_s_BMWiLaval+sent_s_BMWiLevis+sent_s_BMWiMarkham+sent_s_BMWiSainteJulie+sent_s_BMWiSherbrooke+
                    sent_s_BMWiToronto+sent_s_BMWiWestIsland+sent_s_BMWKingston+sent_s_BMWLangley+sent_s_BMWLaval+sent_s_BMWLevis+sent_s_BMWLondon+sent_s_BMWMarkham+
                    sent_s_BMWMoncton+sent_s_BMWNanaimo+sent_s_BMWNewmarket+sent_s_BMWRegina+sent_s_BMWSainteJulie+sent_s_BMWSarnia+sent_s_BMWSherbrooke+sent_s_BMWStJohns+
                    sent_s_BMWToronto+sent_s_BMWVictoria+sent_s_BMWVilleDeQuebec+sent_s_BMWWestIsland+sent_s_BrianJesselBMW+sent_s_BrianJesselBMWPreOw+sent_s_BuddsBMW+
                    sent_s_BuddsBMWHamilton+sent_s_CalgaryBMW+sent_s_CanbecBMW+sent_s_DilawriBMW+sent_s_EdmontonBMW+sent_s_EliteBMW+sent_s_EndrasBMW+sent_s_GeorgianBMW+
                    sent_s_GrenierBMW+sent_s_HamelBMW+sent_s_KelownaBMW+sent_s_MaranelloBMW+sent_s_ORegansBMW+sent_s_Ottos+sent_s_OverseasMotors+sent_s_PalladinoBMW+
                    sent_s_ParkAvenueBMW+sent_s_ParkShoreBMW+sent_s_ParkviewBMW+sent_s_PerformanceBMW+sent_s_PfaffBMW+
                    weekday1_1+weekday1_2+weekday1_3+weekday1_4+weekday1_5+weekday3_3+weekday3_4+weekday3_5+weekday3_6+
                    weekday1_6+weekday2_0+weekday2_1+weekday2_2+weekday2_3+weekday2_4+weekday2_5+weekday2_6+weekday3_1+weekday3_2,
                  data=train_ss,family = "binomial"(link="logit"))
summary(glm_sent_s)
glm_sents_stepwise <-stepAIC(glm_sent_s,direction=c("both"),trace=1)
summary(glm_sents_stepwise)
glm_sent_s <- glm(response~
                    price_0 + price_49 + price_49_62 + price_62_82 + sent_s_Central + 
                    sent_s_Eastern + sent_s_Cash + sent_s_Leas + sent_s_N + sent_s_1 + 
                    sent_s_2 + sent_s_3 + sent_s_4 + sent_s_5 + sent_s_6 + sent_s_7 + 
                    sent_s_8 + sent_s_9 + sent_s_11 + sent_s_2018 + current_term_2 + 
                    current_term_5 + sent_s_BemaAutosport + sent_s_BirchwoodBMW + 
                    sent_s_BMWAutohaus + sent_s_BMWGallery + sent_s_BMWGrandRiver + 
                    sent_s_BMWiAutohaus + sent_s_BMWiLangley + sent_s_AutoWestBMW + 
                    sent_s_BMWiWestIsland + sent_s_BMWKingston + sent_s_BMWLangley + 
                    sent_s_BMWLaval + sent_s_BMWMarkham + sent_s_BMWNanaimo + 
                    sent_s_BMWRegina + sent_s_BMWToronto + sent_s_BMWVilleDeQuebec + 
                    sent_s_BMWWestIsland + sent_s_BrianJesselBMW + sent_s_BrianJesselBMWPreOw + 
                    sent_s_BuddsBMW + sent_s_BuddsBMWHamilton + sent_s_CalgaryBMW + 
                    sent_s_CanbecBMW + sent_s_EliteBMW + sent_s_EndrasBMW + sent_s_GeorgianBMW + 
                    sent_s_GrenierBMW + sent_s_MaranelloBMW + sent_s_Ottos + 
                    sent_s_OverseasMotors + sent_s_ParkAvenueBMW + sent_s_ParkShoreBMW + 
                    sent_s_ParkviewBMW + sent_s_PerformanceBMW + sent_s_PfaffBMW + 
                    weekday1_2 + weekday3_3 + weekday3_4 + weekday3_5 + weekday3_6 + 
                    weekday1_6 + weekday2_0 + weekday2_1 + weekday2_2 + weekday2_3 + 
                    weekday3_2,
                  data=train_ss,family = "binomial"(link="logit"))
summary(glm_sent_s)
pred_glms<-predict(glm_sent_s, test_ss)

pred_glms1<-rep("1",4250)
pred_glms1[pred_glms<mean(pred_glms,0.5)]="0" 
pred_glms1<-as.factor(pred_glms1)
confusionMatrix(pred_glms1,as.factor(test_ss$response),positive = "1")

curve_glms=roc(test_ss$response,pred_glms,smooth=F,ci=T)
curve_glms
############### opt out service  ########################
optout <- cbind(optout, dummy(optout$SurveyType,sep='_'), dummy(optout$Brand,sep='_'), dummy(optout$RegionID,sep='_'), dummy(optout$city,sep='_'), 
                dummy(optout$rec_month,sep='_'), dummy(optout$rec_year,sep='_'),dummy(optout$dealer_name,sep='_'),dummy(optout$province,sep='_'))
test_ind_o <- sample(seq_len(nrow(optout)),size=68000)
test_o <- optout[test_ind_o,]
train_o <- optout[-test_ind_o,]

gbm_opt <- gbm(optout~
                  Metro+customer_only+warranty+customer+campaign+service_days+optout_CustomerPay+optout_Warranty+optout_2018+optout_2019+
                  odo_19+odo_19_34+odo_34_52+odo_52_79+odo_79+cust_0+cust_0_130+cust_130_350+cust_350+war_150+war_150_290+war_290+
                  optout_BMW+optout_BMWi+optout_Central+optout_Eastern+optout_Western+optout_1+optout_2+optout_3+optout_4+
                  optout_5+optout_6+optout_7+optout_8+optout_9+optout_10+optout_11+optout_12+optout_ajax+optout_barrie+optout_blainville+optout_brampton+optout_brossard+
                 optout_calgary+optout_dieppe+optout_dorval+optout_edmonton+optout_halifax+optout_hamilton+optout_kelowna+optout_kingston+optout_kitchener+optout_langley+optout_laval+
                 optout_levis+optout_london+optout_markham+optout_mississauga+optout_montreal+optout_nanaimo+optout_newmarket+optout_northvancouv+optout_northyork+optout_oakville+
                 optout_ottawa+optout_quebec+optout_regina+optout_richmond+optout_richmondhill+optout_rockforest+optout_saintejulie+optout_sarnia+optout_saskatoon+optout_sherbrooke+
                 optout_stcatharine+optout_stcatharines+optout_stjohns+optout_sudbury+optout_terrebonne+optout_thornhill+optout_toronto+optout_troisriviere+optout_vancouver+
                 optout_victoria+optout_winnipeg+optout_woodbridge+optout_AutoWestBMW+optout_BavariaBMW+optout_BemaAutosport+optout_BirchwoodBMW+optout_BMWAutohaus+optout_BMWGallery+
                 optout_BMWGrandRiver+optout_BMWKingston+optout_BMWLangley+optout_BMWLaval+optout_BMWLevis+optout_BMWLondon+optout_BMWMarkham+optout_BMWMoncton+optout_BMWNanaimo+optout_BMWNewmarket+
                 optout_BMWRegina+optout_BMWRichmondHillService+optout_BMWSainteJulie+optout_BMWSarnia+optout_BMWSherbrooke+optout_BMWStJohns+optout_BMWToronto+optout_BMWVictoria+
                 optout_BMWVilleDeQuebec+optout_BMWWestIsland+optout_BrianJesselBMW+optout_BrianJesselBMWPreOwne+optout_BuddsBMW+optout_BuddsBMWHamilton+optout_CalgaryBMW+
                 optout_CanbecBMW+optout_EdmontonBMW+optout_EliteBMW+optout_EndrasBMW+optout_GeorgianBMW+optout_GrenierBMW+optout_HamelAutosDeBlainville+optout_HamelBMW+
                 optout_KelownaBMW+optout_MaranelloBMW+optout_ORegansBMW+optout_Ottos+optout_PalladinoBMW+optout_ParkAvenueBMW+optout_ParkShoreBMW+optout_ParkviewBMW+
                 optout_PerformanceBMW+optout_PerformanceCars+optout_PfaffBMW+optout_PolicaroBMW+optout_SarniaFineCars+optout_TheBMWStore+optout_TroisRivieresBMW+optout_AB+optout_BC+
                 optout_MB+optout_NB+optout_NL+optout_NS+optout_ON+optout_QC+optout_SK,
                data=train_o,
                distribution = "bernoulli",
                n.trees = 60,
                shrinkage = 0.03,
                interaction.depth=7,
                train.fraction = 0.6,
                n.minobsinnode = 500,
                keep.data=TRUE,
                verbose=TRUE,
                n.cores=16)
best.iter_o <- gbm.perf(gbm_opt,method="test")
print(best.iter_o)
summary(gbm_opt, n.trees = best.iter_o)

pred_gbm_o <-predict(gbm_opt, test_o, best.iter_o, type = "response")

output_o <- cbind(test_o, pred_gbm_o)

confusion_gbm_o=ifelse(output_o$pred_gbm_o>mean(output_o$pred_gbm_o,0.5),1,0)
table(output_o$optout, confusion_gbm_o)
curve_gbm_o=roc(output_o$optout,output_o$pred_gbm_o,smooth=F,ci=T)
curve_gbm_o

glm_opt <- glm(optout~
                 Metro+customer_only+warranty+customer+campaign+service_days+optout_CustomerPay+optout_2018+
                 odo_19+odo_19_34+odo_34_52+odo_52_79+cust_0+cust_0_130+cust_130_350+war_150+war_150_290+war_290+
                 optout_BMW+optout_Central+optout_Eastern+optout_Western+optout_1+optout_2+optout_3+optout_4+
                 optout_5+optout_6+optout_7+optout_8+optout_9+optout_10+optout_11+optout_ajax+optout_barrie+optout_blainville+optout_brampton+optout_brossard+
                 optout_calgary+optout_dieppe+optout_dorval+optout_edmonton+optout_halifax+optout_hamilton+optout_kelowna+optout_kingston+optout_kitchener+optout_langley+optout_laval+
                 optout_levis+optout_london+optout_markham+optout_mississauga+optout_montreal+optout_nanaimo+optout_newmarket+optout_northvancouv+optout_northyork+optout_oakville+
                 optout_ottawa+optout_quebec+optout_regina+optout_richmond+optout_richmondhill+optout_rockforest+optout_saintejulie+optout_sarnia+optout_saskatoon+optout_sherbrooke+
                 optout_stcatharine+optout_stcatharines+optout_stjohns+optout_sudbury+optout_terrebonne+optout_thornhill+optout_toronto+optout_troisriviere+optout_vancouver+
                 optout_victoria+optout_winnipeg+optout_BavariaBMW+optout_BMWGallery+
                 optout_BMWSarnia+optout_BrianJesselBMW+optout_BrianJesselBMWPreOwne+optout_EliteBMW+optout_HamelAutosDeBlainville,
               data=train_o,family = "binomial"(link="logit"))
glm(formula = optout ~ Metro + customer_only + warranty + customer + 
      campaign + service_days + optout_CustomerPay + optout_2018 + 
      odo_19 + odo_19_34 + odo_34_52 + odo_52_79 + cust_0 + cust_0_130 + 
      cust_130_350 + war_150 + war_150_290 + war_290 + optout_BMW + 
      optout_Central + optout_Eastern + optout_Western + optout_1 + 
      optout_2 + optout_3 + optout_4 + optout_5 + optout_6 + optout_7 + 
      optout_8 + optout_9 + optout_10 + optout_11 + optout_ajax + 
      optout_barrie + optout_blainville + optout_brampton + optout_brossard + 
      optout_calgary + optout_dieppe + optout_dorval + optout_edmonton + 
      optout_halifax + optout_hamilton + optout_kelowna + optout_kingston + 
      optout_kitchener + optout_langley + optout_laval + optout_levis + 
      optout_london + optout_markham + optout_mississauga + optout_montreal + 
      optout_nanaimo + optout_newmarket + optout_northvancouv + 
      optout_northyork + optout_oakville + optout_ottawa + optout_quebec + 
      optout_regina + optout_richmond + optout_richmondhill + optout_rockforest + 
      optout_saintejulie + optout_sarnia + optout_saskatoon + optout_sherbrooke + 
      optout_stcatharine + optout_stcatharines + optout_stjohns + 
      optout_sudbury + optout_terrebonne + optout_thornhill + optout_toronto + 
      optout_troisriviere + optout_vancouver + optout_victoria + 
      optout_winnipeg + optout_BavariaBMW + optout_BMWGallery + 
      optout_BMWSarnia + optout_BrianJesselBMW + optout_BrianJesselBMWPreOwne + 
      optout_EliteBMW + optout_HamelAutosDeBlainville, family = binomial(link = "logit"), 
    data = train_o)

summary(glm_opt)
glm_opt_stepwise <-stepAIC(glm_opt,direction=c("both"),trace=1)
summary(glm_opt_stepwise)
summary(glm_opt)
pred_glm_opt<-predict(glm_opt, test_o)

pred_glm_opt1<-rep("1",68000)
pred_glm_opt1[pred_glm_opt<mean(pred_glm_opt,0.5)]="0" 
pred_glm_opt1<-as.factor(pred_glm_opt1)
confusionMatrix(pred_glm_opt1,as.factor(test_o$optout),positive = "1")

curve_glm_opt=roc(test_o$optout,pred_glm_opt,smooth=F,ci=T)
curve_glm_opt

#############  optout sales  ######################
term <- dummy(optout_s$current_term,sep='_')
optout_s <- cbind(optout_s,dummy(optout_s$Brand,sep='_'), dummy(optout_s$RegionID,sep='_'), dummy(optout_s$purchase_type,sep='_'), dummy(optout_s$new_cde,sep='_'),
                dummy(optout_s$rec_month,sep='_'), dummy(optout_s$rec_year,sep='_'),term)
test_ind_os <- sample(seq_len(nrow(optout_s)),size=5000)
test_os <- optout_s[test_ind_os,]
train_os <- optout_s[-test_ind_os,]

gbm_sale_os <- gbm(optout~
                    Metro+price_0+price_49+price_49_62+price_62_82+price_82+optout_s_BMW+optout_s_BMWi+optout_s_2018+optout_s_2019+
                    optout_s_Central+optout_s_Eastern+optout_s_Western+optout_s_Cash+optout_s_Leas+optout_s_Loan+optout_s_N+optout_s_U+
                    optout_s_1+optout_s_2+optout_s_3+optout_s_4+optout_s_5+optout_s_6+optout_s_7+optout_s_8+optout_s_9+optout_s_10+optout_s_11+optout_s_12+
                    current_term_0+current_term_1+current_term_2+current_term_3+current_term_4+current_term_5+current_term_6+current_term_7+current_term_8,
                  data=train_os,
                  distribution = "bernoulli",
                  n.trees = 100,
                  shrinkage = 0.01,
                  interaction.depth=6,
                  train.fraction = 0.6,
                  n.minobsinnode = 400,
                  keep.data=TRUE,
                  verbose=TRUE,
                  n.cores=16)
best.iter_os <- gbm.perf(gbm_sale_os,method="test")
print(best.iter_os)
summary(gbm_sale_os, n.trees = best.iter_os)

pred_gbm_os<-predict(gbm_sale_os, test_os, best.iter_os, type = "response")
output_os <- cbind(test_os, pred_gbm_os)
confusion_gbm_os=ifelse(output_os$pred_gbm_os>mean(output_os$pred_gbm_os,0.5),1,0)
table(output_os$optout, confusion_gbm_os)
curve_gbm_os=roc(output_os$optout,output_os$pred_gbm_os,smooth=F,ci=T)
curve_gbm_os

glm_sale_os <- glm(optout~
                     Metro+price_0+price_49+price_49_62+price_62_82+optout_s_BMW+optout_s_2018+
                     optout_s_Central+optout_s_Eastern+optout_s_Western+optout_s_Cash+optout_s_Leas+optout_s_N+
                     optout_s_1+optout_s_2+optout_s_3+optout_s_4+optout_s_5+optout_s_6+optout_s_7+optout_s_8+optout_s_9+optout_s_10+optout_s_11+
                     current_term_1+current_term_2+current_term_3+current_term_4+current_term_5+current_term_6+current_term_7,
                   data=train_os,family = "binomial"(link="logit"))
summary(glm_sale_os)

glm_opts_stepwise <-stepAIC(glm_sale_os,direction=c("both"),trace=1)
summary(glm_opts_stepwise)
glm_sale_os <- glm(optout ~ price_0 + price_49 + price_49_62 + optout_s_BMW + optout_s_2018 + 
                     optout_s_Central + optout_s_Eastern + optout_s_Western + 
                     optout_s_Leas + optout_s_N + optout_s_2 + optout_s_3 + optout_s_4 + 
                     optout_s_7 + optout_s_8 + optout_s_9 + optout_s_10 + optout_s_11 + 
                     current_term_1 + current_term_2 + current_term_6 + current_term_7, family = binomial(link = "logit"), data = train_os)
summary(glm_sale_os)
pred_glm_opts<-predict(glm_sale_os, test_os)

pred_glm_opts1<-rep("1",5000)
pred_glm_opts1[pred_glm_opts<mean(pred_glm_opts,0.5)]="0" 
pred_glm_opts1<-as.factor(pred_glm_opts1)
confusionMatrix(pred_glm_opts1,as.factor(test_os$optout),positive = "1")
curve_glm_opts=roc(test_os$optout,pred_glm_opts,smooth=F,ci=T)
curve_glm_opts

############### days service  ########################
days <- cbind(days, dummy(days$SurveyType,sep='_'), dummy(days$Brand,sep='_'), dummy(days$RegionID,sep='_'), dummy(days$city,sep='_'), 
                dummy(days$rec_month,sep='_'), dummy(days$rec_year,sep='_'),dummy(days$dealer_name,sep='_'),dummy(days$province,sep='_'))
test_ind_d <- sample(seq_len(nrow(days)),size=11500)
test_d <- days[test_ind_d,]
train_d <- days[-test_ind_d,]

gbm_day <- gbm(survey_days~
                 Metro+customer_only+warranty+customer+campaign+service_days+days_CustomerPay+days_Warranty+
                 odo_19+odo_19_34+odo_34_52+odo_52_79+odo_79+cust_0+cust_0_130+cust_130_350+cust_350+war_150+war_150_290+war_290+
                 days_BMW+days_BMWi+days_Central+days_Eastern+days_Western+days_1+days_2+days_3+days_4+days_5+days_6+days_7+days_8+
                 days_9+days_10+days_11+days_12+days_2018+days_2019+days_ajax+days_barrie+days_blainville+days_brampton+days_brossard+
                 days_calgary+days_dieppe+days_dorval+days_edmonton+days_halifax+days_hamilton+days_kelowna+days_kingston+days_kitchener+days_langley+days_laval+
                 days_levis+days_london+days_markham+days_mississauga+days_montreal+days_nanaimo+days_newmarket+days_northvancouv+days_northyork+days_oakville+
                 days_ottawa+days_quebec+days_regina+days_richmond+days_richmondhill+days_rockforest+days_saintejulie+days_sarnia+days_saskatoon+days_sherbrooke+
                 days_stcatharine+days_stcatharines+days_stjohns+days_sudbury+days_terrebonne+days_thornhill+days_toronto+days_troisriviere+days_vancouver+
                 days_victoria+days_winnipeg+days_woodbridge+days_AutoWestBMW+days_BavariaBMW+days_BemaAutosport+days_BirchwoodBMW+days_BMWAutohaus+days_BMWGallery+
                 days_BMWGrandRiver+days_BMWKingston+days_BMWLangley+days_BMWLaval+days_BMWLevis+days_BMWLondon+days_BMWMarkham+days_BMWMoncton+days_BMWNanaimo+days_BMWNewmarket+
                 days_BMWRegina+days_BMWRichmondHillService+days_BMWSainteJulie+days_BMWSarnia+days_BMWSherbrooke+days_BMWStJohns+days_BMWToronto+days_BMWVictoria+
                 days_BMWVilleDeQuebec+days_BMWWestIsland+days_BrianJesselBMW+days_BrianJesselBMWPreOwne+days_BuddsBMW+days_BuddsBMWHamilton+days_CalgaryBMW+
                 days_CanbecBMW+days_EdmontonBMW+days_EliteBMW+days_EndrasBMW+days_GeorgianBMW+days_GrenierBMW+days_HamelAutosDeBlainville+days_HamelBMW+
                 days_KelownaBMW+days_MaranelloBMW+days_ORegansBMW+days_Ottos+days_PalladinoBMW+days_ParkAvenueBMW+days_ParkShoreBMW+days_ParkviewBMW+
                 days_PerformanceBMW+days_PerformanceCars+days_PfaffBMW+days_PolicaroBMW+days_SarniaFineCars+days_TheBMWStore+days_TroisRivieresBMW+days_AB+days_BC+
                 days_MB+days_NB+days_NL+days_NS+days_ON+days_QC+days_SK,
               data=train_d,
               distribution = "gaussian",
               n.trees = 60,
               shrinkage = 0.03,
               interaction.depth=7,
               train.fraction = 0.6,
               n.minobsinnode = 500,
               keep.data=TRUE,
               verbose=TRUE,
               n.cores=16)
best.iter_d <- gbm.perf(gbm_day,method="test")
print(best.iter_d)
summary(gbm_day, n.trees = best.iter_d)

pred_gbm_d <-predict(gbm_day, test_d, best.iter_d, type = "response")
rmse(test_d$survey_days,pred_gbm_d)

glm_days <- glm(survey_days~
                  Metro+customer_only+warranty+customer+campaign+service_days+days_CustomerPay+
                  odo_19+odo_19_34+odo_34_52+odo_52_79+cust_0+cust_0_130+cust_130_350+war_150+war_150_290+war_290+
                  days_BMW+days_Central+days_Eastern+days_1+days_2+days_3+days_4+days_5+days_6+days_7+days_8+
                  days_9+days_10+days_11+days_2018+days_ajax+days_barrie+days_blainville+days_brampton+days_brossard+
                  days_calgary+days_dieppe+days_dorval+days_edmonton+days_halifax+days_hamilton+days_kelowna+days_kingston+days_kitchener+days_langley+days_laval+
                  days_levis+days_london+days_markham+days_mississauga+days_montreal+days_nanaimo+days_newmarket+days_northvancouv+days_northyork+days_oakville+
                  days_ottawa+days_quebec+days_regina+days_richmond+days_richmondhill+days_rockforest+days_saintejulie+days_sarnia+days_saskatoon+days_sherbrooke+
                  days_stcatharine+days_stcatharines+days_stjohns+days_sudbury+days_terrebonne+days_thornhill+days_toronto+days_troisriviere+
                  days_victoria+days_BavariaBMW+days_BMWGallery+days_BMWSarnia+days_BrianJesselBMW+days_BrianJesselBMWPreOwne+days_EliteBMW+days_HamelAutosDeBlainville,
                data=train_d,family = "gaussian")
glm_days <- glm(survey_days~
                  Metro + warranty + campaign + service_days + days_CustomerPay + 
                  odo_19 + odo_19_34 + odo_34_52 + odo_52_79 + cust_0 + war_150 + 
                  war_150_290 + war_290 + days_Eastern + days_1 + days_2 + 
                  days_3 + days_4 + days_5 + days_9 + days_10 + days_2018 + 
                  days_blainville + days_brampton + days_brossard + days_dieppe + 
                  days_dorval + days_edmonton + days_halifax + days_laval + 
                  days_levis + days_markham + days_mississauga + days_montreal + 
                  days_newmarket + days_northvancouv + days_oakville + days_ottawa + 
                  days_quebec + days_richmond + days_rockforest + days_saintejulie + 
                  days_saskatoon + days_sherbrooke + days_stjohns + days_terrebonne + 
                  days_troisriviere + days_victoria + days_BMWSarnia + days_BrianJesselBMW + 
                  days_EliteBMW + days_HamelAutosDeBlainville,
                data=train_d,family = "gaussian")
summary(glm_days)
glm_days_stepwise <-stepAIC(glm_days,direction=c("both"),trace=1)
summary(glm_days_stepwise)

pred_glm_days<-predict(glm_days, test_d)
rmse(test_d$survey_days,pred_glm_days)
#############  days sales  ######################
term <- dummy(days_s$current_term,sep='_')
days_s <- cbind(days_s,dummy(days_s$Brand,sep='_'), dummy(days_s$RegionID,sep='_'), dummy(days_s$purchase_type,sep='_'), dummy(days_s$new_cde,sep='_'),
                dummy(days_s$rec_month,sep='_'), dummy(days_s$rec_year,sep='_'),term)
test_ind_ds <- sample(seq_len(nrow(days_s)),size=1600)
test_ds <- days_s[test_ind_ds,]
train_ds <- days_s[-test_ind_ds,]

gbm_day_s <- gbm(survey_days~
                     Metro+price_0+price_49+price_49_62+price_62_82+price_82+days_s_BMW+days_s_BMWi+days_s_Central+
                     days_s_Eastern+days_s_Western+days_s_Cash+days_s_Leas+days_s_Loan+days_s_N+days_s_U+days_s_1+days_s_2+days_s_3+days_s_4+
                     days_s_5+days_s_6+days_s_7+days_s_8+days_s_9+days_s_10+days_s_11+days_s_12+days_s_2018+days_s_2019+current_term_0+current_term_1+ 
                     current_term_2+current_term_3+current_term_4+current_term_5+current_term_6+current_term_7+current_term_8,
                   data=train_ds,
                   distribution = "gaussian",
                   n.trees = 100,
                   shrinkage = 0.01,
                   interaction.depth=6,
                   train.fraction = 0.6,
                   n.minobsinnode = 300,
                   keep.data=TRUE,
                   verbose=TRUE,
                   n.cores=16)
best.iter_ds <- gbm.perf(gbm_day_s,method="test")
print(best.iter_ds)
summary(gbm_day_s, n.trees = best.iter_ds)

pred_gbm_ds<-predict(gbm_day_s, test_ds, best.iter_ds, type = "response")
rmse(test_ds$survey_days,pred_gbm_ds)

glm_day_s <- glm(survey_days~
                   Metro+price_0+price_49+price_49_62+price_62_82+days_s_BMW+days_s_Central+
                   days_s_Eastern+days_s_Cash+days_s_Leas+days_s_N+days_s_1+days_s_2+days_s_3+days_s_4+
                   days_s_5+days_s_6+days_s_7+days_s_8+days_s_9+days_s_10+days_s_11+days_s_2018+current_term_1+ 
                   current_term_2+current_term_3+current_term_4+current_term_5+current_term_6+current_term_7,
                 data=train_ds,family = "gaussian")
summary(glm_day_s)
glm_day_s_stepwise <-stepAIC(glm_day_s,direction=c("both"),trace=1)
summary(glm_day_s_stepwise)

pred_glm_day_s<-predict(glm_day_s, test_ds)
rmse(test_ds$survey_days,pred_glm_day_s)
