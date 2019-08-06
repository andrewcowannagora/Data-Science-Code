library(readr)
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(sqldf)
library(Hmisc)
library(corrplot)
library("PerformanceAnalytics")
library(mice)
library(VIM)
library(DMwR)
library(rpart)
 
###############################################################################################
####################    Premium fuel lookalike and/or propensity model    #####################
###############################################################################################
 
#import packages;
#install.packages("tidyverse")
install.packages("readr")
library(readr)
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(sqldf)
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)
library(mice)
library(VIM)
library(DMwR)
library(rpart)
 
#################################################################################
##########################    IMPORING DATASET     ##############################
#################################################################################
getwd();
data=read_csv("/Users/mm679j/PremiumFuelCase/PREMIUM_FUEL.csv", col_name=TRUE, na=c("","NA","#NA"));
 
#check # of rows
ncol(data);
nrow(data);
 
#quickly preview data structure with HEAD(), STR(), and SUMMARY()
head(data, 10)
str(data)
summary(data)
 
#For HEAD() to show all columns use:
#options(tibble.width=Inf)
 
#check distribution of target variable
hist(data$TARGET);
summary(data$TARGET);
 
#######################################################################################
######################    DATA EXPLORATION: CATEGORICAL     ###########################
#######################################################################################
 
##########  USING TABLES AND CROSS TABS   #########
#1. Frequency table
freq_tbl=table(CLIENT_GENDER)
head(freq_tbl)
 
#1B. Absolute #s are hard to work with. Let's move to proportion. Input freq table to get prop.
prop.table(freq_tbl)
 
#2. cross-tab for 2 categorical features
freq_xtab=xtabs(~CLIENT_GENDER+TARGET)
 
#2B. cross-tab for 2 categorical features, this time with proportions
prop.table(freq_xtab)
 
 
####################    Using SQL and APPLY      ################################
#explore CLIENT_GENDER w.r.t all other features
#Approach #1: for those comfortable with SQL
sqldf('select CLIENT_GENDER, avg("PARTNERS_SHOPPED") from data group by CLIENT_GENDER')
#learn more here, scroll down has 17 examples: https://github.com/ggrothendieck/sqldf
 
#Approach #2: using base R APPLY
tapply(data$PARTNERS_SHOPPED, data$CLIENT_GENDER, FUN=mean, na.rm=TRUE)
#learn more here: https://www.r-bloggers.com/using-apply-sapply-lapply-in-r/
 
 
####################    Using DPLYR      ########################################
 
#Approach #3: Using ddply from dplyr package
#syntax: ddply(.data, .split_variables, .fun = NULL, ...some other optional stuff)
means = ddply(data,
              c("CLIENT_GENDER"),
              summarise,
              mean_PARTNERS_SHOPPED=mean(PARTNERS_SHOPPED, na.rm=TRUE))
head(means);
 
####################   VISUALIZATIONS   ###############################
freq_tbl=table(CLIENT_GENDER)
 
#1A. barplot with absolute counts
barplot(freq_tbl)
 
#1B. barplot with proportions
barplot(prop.table(freq_tbl))
 
#2. create mosaic plot (plot of proportions) / useful for many levels
mosaicplot(freq_tbl)
 
#Becomes much more useful for categorial variables with many levels
mosaicplot(table(REWARD_HIST_SEGMENT))
 
#3 Visualize
ggplot(data, aes(factor(CLIENT_GENDER), mean_PARTNERS_SHOPPED)) + geom_col()
 
#to learn more go here: http://www.unige.ch/ses/sococ/cl/r/categvar.e.html
 
 
########################################################################################
#######################    DATA EXPLORATION: NUMERIC VARIABLES     #####################
########################################################################################
 
################################ INDEPENDENT EXPLORATION ###############################
#1. Simple summary(data) is very useful for numeric
summary(data)
 
#2 Hisograms are you friend
hist(data$NUM_FMLY_MEMBERS)
 
#3 What if we want to get histograms for all numeric variables?
#This is where DPLYR comes in very handy...
library(reshape2)
library(ggplot2)
 
#Step 1: Get only numeric colums
list_of_numcols = sapply(data, is.numeric)
numcols = data[ , list_of_numcols]
 
#Step 2: Melt data --> Turn it into skinny LONG table
melt_data = melt(numcols, id.vars=c("ID"))
head(melt_data, 10)
 
#Step 3: This data structure is now suitable for a multiplot function
ggplot(data = melt_data, mapping = aes(x = value)) + geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')
#note: need scales = 'free_x' given features are not all on same scale
 
 
###################### RELATIONSHIP WITH TARGET VARIABLE #############################
#What if I tried to just plot it as-is. Two numeric variables, use a regular X-Y scatter plot
ggplot(data, aes(TARGET,AVG_WKLY_SPND_ALL_PARTNERS), na.rm = TRUE) + geom_point()
 
#Approach to explore numeric variables vs binary target
#1. Bin numeric variables (ex: into 10 groups, this is called deciling) to create a factor variable
#2. And then:
    #Create table that shows avg value of target by bin
    #Or visualize on a graph
 
#1. We'll use cut2 function from Hmisc for binning
library(Hmisc)
 
#Before we start, create a categorical version of target variable as well
data = cbind(data, TARGET_CLASS=ifelse(data$TARGET==1, "PREMIUM", "REGULAR"))
head(data)
 
#Binning - create factor
factor_AVG_WKLY_SPND_ALL_PARTNERS = cut2(data$AVG_WKLY_SPND_ALL_PARTNERS, g=10, minmax=TRUE, oneval=TRUE)
data_binned=cbind(data,factor_AVG_WKLY_SPND_ALL_PARTNERS)
head(data_binned)
 
#Create table that shows avg value of target by bin
sqldf('select factor_AVG_WKLY_SPND_ALL_PARTNERS, avg("TARGET") from data_binned group by factor_AVG_WKLY_SPND_ALL_PARTNERS')
 
#Or visualize on a graph
#Create categorical version of label as well (helps with filling)
data = cbind(data, TARGET_CLASS=ifelse(data$TARGET==1, "PREMIUM", "REGULAR"))
head(data)
ggplot(data_binned, aes(factor_AVG_WKLY_SPND_ALL_PARTNERS, ..count..)) + geom_bar(aes(fill = TARGET_CLASS), position = "stack")
 
 
 
######################   RELATIONSHIP WITH OTHER VARIABLES   #############################
#Between all numeric variables...
numeric_cols = sapply(data, is.numeric)
data_num_only=data[,numeric_cols]
 
#But I'll DO IT ONLY ON A SUBSET
data_num_subset = data[,c("NUM_CARS_HH", "NUM_FMLY_MEMBERS", "PROP_WKND", "PROP_WKDAY_DAY","FUEL_TXNS_L12", "AVG_FUEL_VOL_L12")]
head(data_num_subset)
 
#################################### CORRELATOIN MATRIX ##################################
#Correlation Matrix - default one in R
cor(data_num_subset, method = c("pearson"),  use = "complete.obs")
 
#correlation matrix with statistical significance
cor_result=rcorr(as.matrix(data_num_subset))
cor_result$r
 
#Link: http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
The output of the function rcorr() is a list containing the following elements :
r : the correlation matrix
n : the matrix of the number of observations used in analyzing each pair of variables
P : the p-values corresponding to the significance levels of correlations.
 
# Extract the correlation coefficients
cor_result$r
# Extract p-values
cor_result$P
 
# ++++++++++++++++++++++++++++
# flattenCorrMatrix - makes it easier to read (in my opinion)
# ++++++++++++++++++++++++++++
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
 
 
#Simple method to flatten (if that how you want to look at it)
cor_result_flat = flattenCorrMatrix(cor_result$r, cor_result$P)
head(cor_result_flat)
 
 
##################################### CORRELOGRAM #######################################
#The function corrplot() takes the correlation matrix as the first argument. The second argument (type=?upper?) is used to display only the upper triangular of the correlation matrix.
#install.packages("corrplot")
library(corrplot)
corrplot(cor_result$r, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
 
######################### BONUS: Correlation Dashboard ##################################
#warning this will take a few seconds to run..., if computer is slow run only 2 at a time-
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
chart.Correlation(data_num_subset, histogram=TRUE, pch=19)
 
 
 
#######################################################################################
###################    DATA PREPARATION: MISSING VALUES     ###########################
#######################################################################################
 
#Get % of missing values
pMiss = function(x){sum(is.na(x))/length(x)*100}
apply(data,2,pMiss)
 
#install.packages("mice")
library(mice)
 
#Calculates every unique combination of missing data & shows of times that happens
md.pattern(data_num_subset)
 
#Visualize missing data
#install.packages("VIM") #large package install before class
#break dataset into 2 pieces if you have low memory computer...
library(VIM)
aggr_plot = aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
 
 
###################    IMPUTING MISSING DATA     ###########################
####SIMPLE METHODS####
library(Hmisc)
imp_mean=impute(data$NUM_CARS_HH, mean)  # replace with mean
imp_mode=impute(data$NUM_CARS_HH, median)  # median
impute_val=impute(data$NUM_CARS_HH, )  # replace specific number
 
#How welldoes the imputation work?
#well you can back test on the actuals...
#install.packages("DMwR")
library(DMwR)
actuals = data$NUM_CARS_HH[!is.na(data$NUM_CARS_HH)] #get all non-missing values
predicted = rep(mean(data$NUM_CARS_HH, na.rm=T), length(actuals)) #replace them wiht mean and see error
regr.eval(actuals, predicted)
predicted_median = rep(median(data$NUM_CARS_HH, na.rm=T), length(actuals)) #replace them wiht mean and see error
regr.eval(actuals, predicted_median)
 
####R-PART TO PREDICT####
library(rpart)
#For a numeric variable. Ex: NUM_CARS_HH_model
NUM_CARS_HH_model = rpart(NUM_CARS_HH ~ . - TARGET, data=data[!is.na(data$NUM_CARS_HH), ], method="anova", na.action=na.omit)
NUM_CARS_HH_pred = predict(NUM_CARS_HH_model, data[is.na(data$NUM_CARS_HH), ])
 
#Check how well it does...
actuals = data$NUM_CARS_HH[!is.na(data$NUM_CARS_HH)]
regr.eval(actuals, NUM_CARS_HH_pred)
 
 
#kNN Imputation - only numeric
#make sure to remove target variable when doing imputation - when you push model to produciton you want have this!
#also KNN is based on a distance metric so requires normalization/scaling
knn_input=as.data.frame(data_num_only[, !names(data_num_only) %in% "TARGET"]) #was giving error was tibble, so tried again after converting to data.frame()
head(knn_input)
 
#takes long to run, won't do it in class
sample_knn_input=sample_n(knn_input, 10000)
knnOutput = knnImputation(sample_knn_input, k=3)
 
 
 
#######################################################################################
###################    DATA PREPARATION: OUTLIERS VALUES     ##########################
#######################################################################################
#outlier - simple box plots...
boxplot(data$AVG_WKLY_SPND_ALL_PARTNERS, las = 1, ylim=c(-50, 200))
boxplot(data$AVG_FUEL_VOL_L12, las = 1, ylim=c(-50, 200))
 
#bivariate...outliers by region etc.
boxplot(AVG_FUEL_VOL_L12 ~ CLIENT_REGION, data=data, main="Premium fuel across regions")
#clear pattern is noticeable.
 
# For continuous variable outlier within a group (bivariate)
boxplot(data$AVG_FUEL_VOL_L12, data=data)
 
boxplot(data$ENDING_PT_BALANCE, data=data)
boxplot(ENDING_PT_BALANCE ~ REWARD_HIST_SEGMENT, data=data)
boxplot(NUM_CARS_HH ~ cut(AVG_FUEL_VOL_L12, pretty(data$AVG_FUEL_VOL_L12)), data=data, cex.axis=0.5)
#no clear pattern...
 
#Common ways of dealing with outliers
#Imputing and/or predictoin which we covered before
 
 
########################################################
# R Function for Outlier Treatment : Percentile Capping
########################################################
pcap <- function(x){
  for (i in which(sapply(x, is.numeric))) {
    quantiles <- quantile( x[,i], c(.05, .95 ), na.rm =TRUE)
    x[,i] = ifelse(x[,i] < quantiles[1] , quantiles[1], x[,i])
    x[,i] = ifelse(x[,i] > quantiles[2] , quantiles[2], x[,i])}
  x}
 
# Replacing extreme values with percentiles
data_new = pcap(data_num_subset)
summary(data$AVG_FUEL_VOL_L12)
summary(data_new$AVG_FUEL_VOL_L12)
 
# Checking Percentile values of 7th variable
quantile(abcd[,7], c(0.25,0.5,.95, .99, 1), na.rm = TRUE)

########################################################
# R Function for Outlier Treatment : Percentile Capping
########################################################
install.packages("randomForest")
library(randomForest)

#install packages
install.packages("caret")
install.packages("caret", dependencies=c("Depends", "Suggests"))
library(caret)


set.seed(7)
fit.cart <- train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)



#load data
data(iris)
# rename the dataset
dataset = iris

# load data as if it was an external file
filename <- "iris.csv"
# load the CSV file from the local directory
dataset <- read.csv(filename, header=FALSE)
# set the column names in the dataset
colnames(dataset) <- c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species")

#create train/validation dataset
# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(dataset$Species, p=0.80, list=FALSE)

# select 20% of the data for validation
validation <- dataset[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataset <- dataset[validation_index,]


#DATA EXPLORATION
# dimensions of dataset
dim(dataset)

# list types for each attribute
sapply(dataset, class)

# take a peek at the first 5 rows of the data
head(dataset)

# list the levels for the class
levels(dataset$Species)

# summarize the class distribution
percentage <- prop.table(table(dataset$Species)) * 100
cbind(freq=table(dataset$Species), percentage=percentage)


# summarize attribute distributions
summary(dataset)

#DATA VISUALIZATION

#univariate plots

# split input and output
x <- dataset[,1:4]
y <- dataset[,5]

# boxplot for each attribute on one image
par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(x[,i], main=names(iris)[i])
}

# barplot for class breakdown
plotYes


#multivatiate plots
# scatterplot matrix
featurePlot(x=x, y=y, plot="ellipse")

# box and whisker plots for each attribute
featurePlot(x=x, y=y, plot="box")

# density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)


#ML ALGORITHMS
# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

#5 different models
# a)LDA 
set.seed(7)
fit.lda <- train(Species~., data=dataset, method="lda", metric=metric, trControl=control)

# CART decision tree
set.seed(7)
fit.cart <- train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)

# kNN
set.seed(7)
fit.knn <- train(Species~., data=dataset, method="knn", metric=metric, trControl=control)

# SVM
set.seed(7)
fit.svm <- train(Species~., data=dataset, method="svmRadial", metric=metric, trControl=control)

# Random Forest
set.seed(7)
fit.rf <- train(Species~., data=dataset, method="rf", metric=metric, trControl=control)


#MODEL SELECTION
# summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

# compare accuracy of models
dotplot(results)

# summarize Best Model (LDA)
print(fit.lda)


#MODEL DEPLOYMENT / MAKE PREDICTIONS
# estimate skill of LDA on the validation dataset
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)






drops <- c("TARGET","ID")
train_new = training[ , !(names(training) %in% drops)]

train_set <- train_new[, colSums(is.na(train_new)) == 0]

str(train_set)

#Train Random Forest
rf <-randomForest(TARGET_CLASS ~.,data=train_new, importance=TRUE,ntree=20)

#Evaluate variable importance
imp = importance(rf, type=1)
imp = data.frame(predictors=rownames(imp),imp)

# Order the predictor levels by importance
imp.sort <- arrange(imp,desc(MeanDecreaseAccuracy))

imp.sort$predictors <- factor(imp.sort$predictors,levels=imp.sort$predictors)

# Select the top 20 predictors
imp.20<- imp.sort[1:20,]
print(imp.20)

# Plot Important Variables
varImpPlot(rf, type=1)

# Subset data with 20 independent and 1 dependent variables
dat4 = cbind(classe = dat3$classe, dat3[,c(imp.20$predictors)])


