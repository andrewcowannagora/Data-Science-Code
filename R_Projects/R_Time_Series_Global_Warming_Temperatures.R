##################################################################
#Global Warming Revisited(A) UVA-QA-0808

#This file uses the dataset published by NASA for monthly GLOBAL 
#temperate from January 1880 through March 2019
##################################################################

#Loading the packages
library(ggplot2)
library(forecast)
library(fpp)
library(fpp2)
library(sqldf)


#Data Sets

#1. GISTEMP Team, 2019: GISS Surface Temperature Analysis (GISTEMP). NASA Goddard Institute for Space Studies. 
#   Dataset accessed 2019-04-29 at https://data.giss.nasa.gov/gistemp/. 
#   This has been referred as 'NASA' throughout the document.

#2. Climatic Research Unit (University of East Anglia) in conjunction with the Hadley Centre (UK Met Office). 
#   https://crudata.uea.ac.uk/cru/data/temperature/. 
#   This has been referred as 'UK Met' throughout the document.


#3. Co2 emission data has been obtained from NOAA-ESRL Global Monitoring at C02.earth. 
#   The unit of measure is parts per million (ppm) which measures the concentration of carbon in the atmosphere: https://www.co2.earth/monthly-co2#noaa. 

#4. Kingston Weather Data: http://climate.weather.gc.ca/historical_data/search_historic_data_e.html






#Load the datafile
nasa <- read.csv(file.choose())
nasa$temp <- 14 + nasa$Anomaly
names(nasa) <- c("Year", "Month", "Anomaly", "temp")
nasa$Year <- as.numeric(nasa$Year)
str(nasa)

######################
#Construct Time Series
######################
nasa.ts <- ts(nasa[,4], frequency=12, start=c(1880,1))
nasa.train <- window(nasa.ts, end=c(2006,12))

######################
#Preliminary Analysis
######################
nasa$decade <- ifelse(nasa$Year <= 1890, "1880-1890", 
                      ifelse(nasa$Year <= 1900, "1890-1900",
                             ifelse(nasa$Year <= 1910, "1900-1910", 
                                    ifelse(nasa$Year <= 1920, "1910-1920",
                                           ifelse(nasa$Year <= 1930, "1920-1930",
                                                  ifelse(nasa$Year <= 1940, "1930-1940",
                                                         ifelse(nasa$Year <= 1950, "1940-1950",
                                                                ifelse(nasa$Year <= 1960, "1950-1960",
                                                                       ifelse(nasa$Year <= 1970, "1960-1970",
                                                                              ifelse(nasa$Year <= 1980, "1970-1980",
                                                                                     ifelse(nasa$Year <= 1990, "1980-1990",
                                                                                            ifelse(nasa$Year <= 2000, "1990-2000",
                                                                                                   ifelse(nasa$Year <= 2010, "2000-2010",
                                                                                                          "2010-2019")))))))))))))

nasa$decade <- as.factor(nasa$decade)

ggplot(nasa, aes(x= decade, y=temp, fill = decade)) + geom_boxplot(aes(middle=mean(temp))) + xlab("Decade") + 
  ylab("Average Mean Temperature") + theme_bw() + ggtitle("Average Mean Temperature by Decade") + geom_line() + 
  scale_x_discrete(label=abbreviate) + labs(caption ="Datasource: NASA Goddard Institute for Space Studies (GISTEMP)")

autoplot(nasa.ts) + theme_bw() + ylab("Average Temperature") + xlab("Year") + 
  ggtitle("Global Mean Temperature 1880-2019") + labs(caption ="Datasource: NASA Goddard Institute for Space Studies (GISTEMP)")

# ggseasonplot(nasa.ts)

# ggseasonplot(nasa.ts, polar=TRUE)

ggsubseriesplot(nasa.ts) + theme_bw() + xlab("Year") + ylab("Global Temperature") + 
  ggtitle("Sub Series Plot for Global Temperature (1880-2019)", sub="Blue line represents mean for the month") + 
  labs(caption ="Datasource: NASA Goddard Institute for Space Studies (GISTEMP)")

gglagplot(nasa.ts) + ggtitle("Lag Plot of Global Mean Temperature (1880-2019)") + xlab("Global Mean Temperature") +
  ylab("Global Mean Temperature") + labs(caption ="Datasource: NASA Goddard Institute for Space Studies (GISTEMP)")

ggAcf(nasa.ts) + ggtitle("Acf plot for Global Mean Temperature (1880-2019)") + theme_bw() + 
  labs(caption ="Datasource: NASA Goddard Institute for Space Studies (GISTEMP)")

ggPacf(nasa.ts) + ggtitle("Pacf plot for Global Mean Temperature (1880-2019)") + theme_bw() + 
  labs(caption ="Datasource: NASA Goddard Institute for Space Studies (GISTEMP)")

##################################################
#Simple Linear Regression Model on ~ time(nasa.ts)
##################################################
nasa.ts.lm <- lm(nasa.ts ~ time(nasa.ts))
par(mfrow=c(1,4))
plot(nasa.ts.lm)
# mean(nasa.ts.lm$residuals)

#Decomposition
decomp.nasa.add <- decompose(nasa.ts, type="additive")
decomp.nasa.mult <- decompose(nasa.ts, type="multiplicative")
plot(decomp.nasa.add)
plot(decomp.mult)

#Create benchmark models on nasa.ts3
nasa.ts2 <- window(nasa.ts, start=1980) #create benchmark models on 1980 and onwards
autoplot(nasa.ts2) + theme_bw() + ylab("Average Temperature") + 
  ggtitle("Global Mean Temperature and Benchmark Forecasting Models", sub="Actual: 1980 - 2019, Forecast: 2019-2029") + 
  autolayer(meanf(nasa.ts2, h=120),series="Mean", PI=FALSE) + autolayer(rwf(nasa.ts2, h=120),series="Naive", PI=FALSE) + 
  autolayer(snaive(nasa.ts2, h=120),series="Seasonal Naive", PI=FALSE) + xlab("Year") + labs(caption ="Datasource: NASA Goddard Institute for Space Studies (GISTEMP)")

# Create exponential smoothing models (ETS)
# This will also be used a cross train 1 set 
#(train period 1/1880 - 12/2006, test period 1/2007 - 3/2019)

#First model 'AAA' (Also a cross validate 1 set)
nasa.AAA <- ets(nasa.train, model="AAA")
nasa.AAA.pred <- forecast(nasa.AAA, h=147, level=0.9)
plot(nasa.AAA.pred, xlab="Year", ylab="Global Mean Temperature")
accuracy(nasa.AAA)
checkresiduals(nasa.AAA)
mean(nasa.AAA$residuals)

#First model 'MMM' (Also a cross validate 1 set)
nasa.MMM <- ets(nasa.train, model="MMM")
nasa.MMM.pred <- forecast(nasa.MMM, h=147, level=0.9)
plot(nasa.MMM.pred, xlab="Year", ylab="Global Mean Temperature")
accuracy(nasa.MMM)
checkresiduals(nasa.MMM)
mean(nasa.MMM$residuals)

#First model 'ZZZ' - autotransformed into 'ANN' (Also a cross validate 1 set)
nasa.ZZZ <- ets(nasa.train, model="ZZZ")
nasa.ZZZ.pred <- forecast(nasa.ZZZ, h=147, level=0.9)
plot(nasa.ZZZ.pred, xlab="Year", ylab="Global Mean Temperature")
accuracy(nasa.ZZZ)
checkresiduals(nasa.ZZZ)
mean(nasa.ZZZ$residuals)

# Create a trigonometric box-cox autoregressive trend seasonality (TBATS) model (Also a cross validate 1 set)
nasa.tbats <- tbats(nasa.train)
nasa.tbats.pred <- forecast(nasa.tbats, h=147, level=0.9)
plot(nasa.tbats.pred, xlab="Year", ylab="Global Mean Temperature")
accuracy(nasa.tbats)
checkresiduals(nasa.tbats)
mean(nasa.tbats$errors)

#ARIMA model (Also a cross validate 1 set)
nasa.arima <- auto.arima(nasa.train, seasonal=TRUE)
nasa.arima.pred <-forecast(nasa.arima, h=147, level=0.9)
accuracy(nasa.arima)
checkresiduals(nasa.arima)
mean(nasa.arima$residuals)

par(mfrow=c(1,5)) # Lets look at the three models with seasonality on one graph on the same scale
plot(nasa.AAA.pred, xlab="Year", ylab="Global Mean Temperature", ylim=c(11,18), include=500)
plot(nasa.MMM.pred, xlab="Year", ylab="Global Mean Temperature", ylim=c(11,18), include=500)
plot(nasa.ZZZ.pred, xlab="Year", ylab="Global Mean Temperature", ylim=c(11,18), include=500)
plot(nasa.tbats.pred, xlab="Year", ylab="Global Mean Temperature", ylim=c(11,18), include=500)
plot(nasa.arima.pred, xlab="Year", ylab="Global Mean Temperature", ylim=c(11,18), include=500)

#write models to csv
write.csv(nasa.AAA.pred, file = "NASA AAA Forecast.csv")
write.csv(nasa.MMM.pred, file = "NASA MMM Forecast.csv")
write.csv(nasa.ZZZ.pred, file = "NASA ANN Forecast.csv")
write.csv(nasa.tbats.pred, file = "NASA TBATS Forecast.csv")
write.csv(nasa.arima.pred, file = "NASA ARIMA Forecast.csv")

#Cross Validation
#First model built above is CV1
nasa.train2 <- window(nasa.ts, end=c(1996,12))
nasa.train3 <- window(nasa.ts, end=c(1986,12))
nasa.train4 <- window(nasa.ts, end=c(1976,12))
nasa.train5 <- window(nasa.ts, end=c(1966,12))

#AAA Models (CV2, CV3, CV4, CV5)

nasa.AAA.cv2 <- ets(nasa.train2, model="AAA")
nasa.AAA.cv2.pred <- forecast(nasa.AAA.cv2, h=267, level=0.9)
write.csv(nasa.AAA.cv2.pred, file = "NASA AAA cv2.csv")

nasa.AAA.cv3 <- ets(nasa.train3, model="AAA")
nasa.AAA.cv3.pred <- forecast(nasa.AAA.cv3, h=387, level=0.9)
write.csv(nasa.AAA.cv3.pred, file = "NASA AAA cv3.csv")

nasa.AAA.cv4 <- ets(nasa.train4, model="AAA")
nasa.AAA.cv4.pred <- forecast(nasa.AAA.cv4, h=507, level=0.9)
write.csv(nasa.AAA.cv4.pred, file = "NASA AAA cv4.csv")

nasa.AAA.cv5 <- ets(nasa.train5, model="AAA")
nasa.AAA.cv5.pred <- forecast(nasa.AAA.cv5, h=627, level=0.9)
write.csv(nasa.AAA.cv5.pred, file = "NASA AAA cv5.csv")

#MMM Models (CV2, CV3, CV4, CV5)

nasa.MMM.cv2 <- ets(nasa.train2, model="MMM")
nasa.MMM.cv2.pred <- forecast(nasa.MMM.cv2, h=267, level=0.9)
write.csv(nasa.MMM.cv2.pred, file = "NASA MMM cv2.csv")

nasa.MMM.cv3 <- ets(nasa.train3, model="MMM")
nasa.MMM.cv3.pred <- forecast(nasa.MMM.cv3, h=387, level=0.9)
write.csv(nasa.MMM.cv3.pred, file = "NASA MMM cv3.csv")

nasa.MMM.cv4 <- ets(nasa.train4, model="MMM")
nasa.MMM.cv4.pred <- forecast(nasa.MMM.cv4, h=507, level=0.9)
write.csv(nasa.MMM.cv4.pred, file = "NASA MMM cv4.csv")

nasa.MMM.cv5 <- ets(nasa.train5, model="MMM")
nasa.MMM.cv5.pred <- forecast(nasa.MMM.cv5, h=627, level=0.9)
write.csv(nasa.MMM.cv5.pred, file = "NASA MMM cv5.csv")

#ANN Models (CV2, CV3, CV4, CV5)

nasa.ANN.cv2 <- ets(nasa.train2, model="ANN")
nasa.ANN.cv2.pred <- forecast(nasa.ANN.cv2, h=267, level=0.9)
write.csv(nasa.ANN.cv2.pred, file = "NASA ANN cv2.csv")

nasa.ANN.cv3 <- ets(nasa.train3, model="ANN")
nasa.ANN.cv3.pred <- forecast(nasa.ANN.cv3, h=387, level=0.9)
write.csv(nasa.ANN.cv3.pred, file = "NASA ANN cv3.csv")

nasa.ANN.cv4 <- ets(nasa.train4, model="ANN")
nasa.ANN.cv4.pred <- forecast(nasa.ANN.cv4, h=507, level=0.9)
write.csv(nasa.ANN.cv4.pred, file = "NASA ANN cv4.csv")

nasa.ANN.cv5 <- ets(nasa.train5, model="ANN")
nasa.ANN.cv5.pred <- forecast(nasa.ANN.cv5, h=627, level=0.9)
write.csv(nasa.ANN.cv5.pred, file = "NASA ANN cv5.csv")

#tbats model (CV2, CV3, CV4, CV5)

nasa.tbats.cv2 <- tbats(nasa.train2)
nasa.tbats.cv2.pred <- forecast(nasa.tbats.cv2, h=267, level=0.9)
write.csv(nasa.tbats.cv2.pred, file = "NASA TBATS cv2.csv")

nasa.tbats.cv3 <- tbats(nasa.train3)
nasa.tbats.cv3.pred <- forecast(nasa.tbats.cv3, h=387, level=0.9)
write.csv(nasa.tbats.cv3.pred, file = "NASA TBATS cv3.csv")

nasa.tbats.cv4 <- tbats(nasa.train4)
nasa.tbats.cv4.pred <- forecast(nasa.tbats.cv4, h=507, level=0.9)
write.csv(nasa.tbats.cv4.pred, file = "NASA TBATS cv4.csv")

nasa.tbats.cv5 <- tbats(nasa.train5)
nasa.tbats.cv5.pred <- forecast(nasa.tbats.cv5, h=627, level=0.9)
write.csv(nasa.tbats.cv5.pred, file = "NASA TBATS cv5.csv")

#arima model (CV2, CV3, CV4, CV5)
nasa.arima.cv2 <- auto.arima(nasa.train2, seasonal=TRUE)
nasa.arima.cv2.pred <-forecast(nasa.arima.cv2, h=267, level=0.9)
write.csv(nasa.arima.cv2.pred, file = "NASA arima cv2.csv")

nasa.arima.cv3 <- auto.arima(nasa.train3, seasonal=TRUE)
nasa.arima.cv3.pred <-forecast(nasa.arima.cv3, h=387, level=0.9)
write.csv(nasa.arima.cv3.pred, file = "NASA arima cv3.csv")

nasa.arima.cv4 <- auto.arima(nasa.train4, seasonal=TRUE)
nasa.arima.cv4.pred <-forecast(nasa.arima.cv4, h=507, level=0.9)
write.csv(nasa.arima.cv4.pred, file = "NASA arima cv4.csv")

nasa.arima.cv5 <- auto.arima(nasa.train5, seasonal=TRUE)
nasa.arima.cv5.pred <-forecast(nasa.arima.cv5, h=627, level=0.9)
write.csv(nasa.arima.cv5.pred, file = "NASA arima cv5.csv")


#########################
####   FINAL MODEL   ####
#########################

#Since ARIMA tends to better model in all cross validation
#We will now answer Q1 and Q2 using arima model.

#ARIMA model
nasa.arima.final <- auto.arima(nasa.ts, seasonal=TRUE)
nasa.arima.final.pred <-forecast(nasa.arima.final, h=981, level=0.9)
plot(nasa.arima.final.pred, xlab="Year", ylab="Global Mean Temperature", ylim=c(11,18), include=500)
checkresiduals(nasa.arima)
mean(nasa.arima$residuals)
write.csv(nasa.arima.final.pred, file = "NASA arima final pred 2100.csv")



##################################################################
#Assignment # 2 MMA 867 Predictive Modeling - Due on May 11, 2019
#Global Warming Revisited(A) UVA-QA-0808
#Team Bader

#This file uses the dataset published by UKMET for monthly GLOBAL 
#temperate from January 1880 through March 2019
##################################################################

#Loading the packages
library(ggplot2)
library(forecast)
library(fpp)
library(fpp2)
library(sqldf)

#Load the datafile

ukmet <- read.csv(file.choose())
ukmet$temp <- 14 + ukmet$Anomaly
names(ukmet) <- c("Year", "Month", "Anomaly", "temp")
ukmet$Year <- as.numeric(ukmet$Year)
ukmet <- ukmet[ukmet$Year >= 1880, ]
str(ukmet)
head(ukmet)

######################
#Construct Time Series
######################
ukmet.ts <- ts(ukmet[,4], frequency=12, start=c(1880,1))
ukmet.train <- window(ukmet.ts, end=c(2006,12))

######################
#Preliminary Analysis
######################
ukmet$decade <- ifelse(ukmet$Year <= 1890, "1880-1890", 
                       ifelse(ukmet$Year <= 1900, "1890-1900",
                              ifelse(ukmet$Year <= 1910, "1900-1910", 
                                     ifelse(ukmet$Year <= 1920, "1910-1920",
                                            ifelse(ukmet$Year <= 1930, "1920-1930",
                                                   ifelse(ukmet$Year <= 1940, "1930-1940",
                                                          ifelse(ukmet$Year <= 1950, "1940-1950",
                                                                 ifelse(ukmet$Year <= 1960, "1950-1960",
                                                                        ifelse(ukmet$Year <= 1970, "1960-1970",
                                                                               ifelse(ukmet$Year <= 1980, "1970-1980",
                                                                                      ifelse(ukmet$Year <= 1990, "1980-1990",
                                                                                             ifelse(ukmet$Year <= 2000, "1990-2000",
                                                                                                    ifelse(ukmet$Year <= 2010, "2000-2010",
                                                                                                           "2010-2019")))))))))))))

ukmet$decade <- as.factor(ukmet$decade)

ggplot(ukmet, aes(x= decade, y=temp, fill = decade)) + geom_boxplot(aes(middle=mean(temp))) + xlab("Decade") + 
  ylab("Average Mean Temperature") + theme_bw() + ggtitle("Average Mean Temperature by Decade") + geom_line() + 
  scale_x_discrete(label=abbreviate) + labs(caption ="Datasource: UK Met Office (HadCRUT4)")

autoplot(ukmet.ts) + theme_bw() + ylab("Average Temperature") + xlab("Year") + ggtitle("Global Mean Temperature 1880-2019") + 
  labs(caption ="Datasource: UK Met Office (HadCRUT4)")

# ggseasonplot(ukmet.ts)

# ggseasonplot(ukmet.ts, polar=TRUE)

ggsubseriesplot(ukmet.ts) + theme_bw() + xlab("Year") + ylab("Global Temperature") + 
  ggtitle("Sub Series Plot for Global Temperature (1880-2019)", sub="Blue line represents mean for the month") + 
  labs(caption ="Datasource: UK Met Office (HadCRUT4)")

gglagplot(ukmet.ts) + ggtitle("Lag Plot of Global Mean Temperature (1880-2019)") + xlab("Global Mean Temperature") +
  ylab("Global Mean Temperature") + labs(caption ="Datasource: UK Met Office (HadCRUT4)")

ggAcf(ukmet.ts) + ggtitle("Acf plot for Global Mean Temperature (1880-2019)") + theme_bw() + labs(caption ="Datasource: UK Met Office (HadCRUT4)")

ggPacf(ukmet.ts) + ggtitle("Pacf plot for Global Mean Temperature (1880-2019)") + theme_bw() + labs(caption ="Datasource: UK Met Office (HadCRUT4)")


##################################################
#Simple Linear Regression Model on ~ time(ukmet.ts)
##################################################
ukmet.ts.lm <- lm(ukmet.ts ~ time(ukmet.ts))
par(mfrow=c(1,4))
plot(ukmet.ts.lm)
# mean(ukmet.ts.lm$residuals)

#Decomposition
decomp.ukmet.add <- decompose(ukmet.ts, type="additive")
decomp.ukmet.mult <- decompose(ukmet.ts, type="multiplicative")
plot(decomp.ukmet.add)
plot(decomp.ukmet.mult)

#Create benchmark models on ukmet.ts3
ukmet.ts2 <- window(ukmet.ts, start=1980) #create benchmark models on 1980 and onwards
autoplot(ukmet.ts2) + theme_bw() + ylab("Average Temperature") + 
  ggtitle("Global Mean Temperature and Benchmark Forecasting Models", sub="Actual: 1980 - 2019, Forecast: 2019-2029") + 
  autolayer(meanf(ukmet.ts2, h=120),series="Mean", PI=FALSE) + autolayer(rwf(ukmet.ts2, h=120),series="Naive", PI=FALSE) + 
  autolayer(snaive(ukmet.ts2, h=120),series="Seasonal Naive", PI=FALSE) + xlab("Year") + 
  labs(caption ="Datasource: UK Met Office (HadCRUT4)")

# Create exponential smoothing models (ETS)
# This will also be used a cross train 1 set 
#(train period 1/1880 - 12/2006, test period 1/2007 - 3/2019)

#First model 'AAA' (Also a cross validate 1 set)
ukmet.AAA <- ets(ukmet.train, model="AAA")
ukmet.AAA.pred <- forecast(ukmet.AAA, h=146, level=0.9)
plot(ukmet.AAA.pred, xlab="Year", ylab="Global Mean Temperature")
accuracy(ukmet.AAA)
checkresiduals(ukmet.AAA)
mean(ukmet.AAA$residuals)

#First model 'MMM' (Also a cross validate 1 set)
ukmet.MMM <- ets(ukmet.train, model="MMM")
ukmet.MMM.pred <- forecast(ukmet.MMM, h=146, level=0.9)
plot(ukmet.MMM.pred, xlab="Year", ylab="Global Mean Temperature")
accuracy(ukmet.MMM)
checkresiduals(ukmet.MMM)
mean(ukmet.MMM$residuals)

# # First model 'ZZZ' - autotransformed into 'ANA' (Also a cross validate 1 set) Ignore for remaining script
# ukmet.ZZZ <- ets(ukmet.train, model="ZZZ")
# ukmet.ZZZ.pred <- forecast(ukmet.ZZZ, h=146, level=0.9)
# plot(ukmet.ZZZ.pred, xlab="Year", ylab="Global Mean Temperature")
# accuracy(ukmet.ZZZ)
# checkresiduals(ukmet.ZZZ)
# mean(ukmet.ZZZ$residuals)

# First model 'ANN' (Also a cross validate 1 set)
ukmet.ANN <- ets(ukmet.train, model="ANN")
ukmet.ANN.pred <- forecast(ukmet.ANN, h=146, level=0.9)
plot(ukmet.ANN.pred, xlab="Year", ylab="Global Mean Temperature")
accuracy(ukmet.ANN)
checkresiduals(ukmet.ANN)
mean(ukmet.ANN$residuals)

# Create a trigonometric box-cox autoregressive trend seasonality (TBATS) model (Also a cross validate 1 set)
ukmet.tbats <- tbats(ukmet.train)
ukmet.tbats.pred <- forecast(ukmet.tbats, h=146, level=0.9)
plot(ukmet.tbats.pred, xlab="Year", ylab="Global Mean Temperature")
accuracy(ukmet.tbats)
checkresiduals(ukmet.tbats)
mean(ukmet.tbats$errors)

#ARIMA model (Also a cross validate 1 set)
ukmet.arima <- auto.arima(ukmet.train, seasonal=TRUE)
ukmet.arima.pred <-forecast(ukmet.arima, h=146, level=0.9)
plot(ukmet.arima.pred, xlab="Year", ylab="Global Mean Temperature")
accuracy(ukmet.arima)
checkresiduals(ukmet.arima)
mean(ukmet.arima$residuals)

par(mfrow=c(1,5)) # Lets look at the three models with seasonality on one graph on the same scale
plot(ukmet.AAA.pred, xlab="Year", ylab="Global Mean Temperature", ylim=c(11,18), include=500)
plot(ukmet.MMM.pred, xlab="Year", ylab="Global Mean Temperature", ylim=c(11,18), include=500)
plot(ukmet.ZZZ.pred, xlab="Year", ylab="Global Mean Temperature", ylim=c(11,18), include=500)
plot(ukmet.tbats.pred, xlab="Year", ylab="Global Mean Temperature", ylim=c(11,18), include=500)
plot(ukmet.arima.pred, xlab="Year", ylab="Global Mean Temperature", ylim=c(11,18), include=500)

#write models to csv
write.csv(ukmet.AAA.pred, file = "ukmet AAA Forecast.csv")
write.csv(ukmet.MMM.pred, file = "ukmet MMM Forecast.csv")
write.csv(ukmet.ANN.pred, file = "ukmet ANN Forecast.csv")
write.csv(ukmet.tbats.pred, file = "ukmet TBATS Forecast2.csv")
write.csv(ukmet.arima.pred, file = "ukmet ARIMA Forecast.csv")

#Cross Validation
#First model built above is CV1
ukmet.train2 <- window(ukmet.ts, end=c(1996,12))
ukmet.train3 <- window(ukmet.ts, end=c(1986,12))
ukmet.train4 <- window(ukmet.ts, end=c(1976,12))
ukmet.train5 <- window(ukmet.ts, end=c(1966,12))

#AAA Models (CV2, CV3, CV4, CV5)

ukmet.AAA.cv2 <- ets(ukmet.train2, model="AAA")
ukmet.AAA.cv2.pred <- forecast(ukmet.AAA.cv2, h=267, level=0.9)
write.csv(ukmet.AAA.cv2.pred, file = "ukmet AAA cv2.csv")

ukmet.AAA.cv3 <- ets(ukmet.train3, model="AAA")
ukmet.AAA.cv3.pred <- forecast(ukmet.AAA.cv3, h=387, level=0.9)
write.csv(ukmet.AAA.cv3.pred, file = "ukmet AAA cv3.csv")

ukmet.AAA.cv4 <- ets(ukmet.train4, model="AAA")
ukmet.AAA.cv4.pred <- forecast(ukmet.AAA.cv4, h=507, level=0.9)
write.csv(ukmet.AAA.cv4.pred, file = "ukmet AAA cv4.csv")

ukmet.AAA.cv5 <- ets(ukmet.train5, model="AAA")
ukmet.AAA.cv5.pred <- forecast(ukmet.AAA.cv5, h=627, level=0.9)
write.csv(ukmet.AAA.cv5.pred, file = "ukmet AAA cv5.csv")

#MMM Models (CV2, CV3, CV4, CV5)

ukmet.MMM.cv2 <- ets(ukmet.train2, model="MMM")
ukmet.MMM.cv2.pred <- forecast(ukmet.MMM.cv2, h=267, level=0.9)
write.csv(ukmet.MMM.cv2.pred, file = "ukmet MMM cv2.csv")

ukmet.MMM.cv3 <- ets(ukmet.train3, model="MMM")
ukmet.MMM.cv3.pred <- forecast(ukmet.MMM.cv3, h=387, level=0.9)
write.csv(ukmet.MMM.cv3.pred, file = "ukmet MMM cv3.csv")

ukmet.MMM.cv4 <- ets(ukmet.train4, model="MMM")
ukmet.MMM.cv4.pred <- forecast(ukmet.MMM.cv4, h=507, level=0.9)
write.csv(ukmet.MMM.cv4.pred, file = "ukmet MMM cv4.csv")

ukmet.MMM.cv5 <- ets(ukmet.train5, model="MMM")
ukmet.MMM.cv5.pred <- forecast(ukmet.MMM.cv5, h=627, level=0.9)
write.csv(ukmet.MMM.cv5.pred, file = "ukmet MMM cv5.csv")

#ANN Models (CV2, CV3, CV4, CV5)

ukmet.ANN.cv2 <- ets(ukmet.train2, model="ANN")
ukmet.ANN.cv2.pred <- forecast(ukmet.ANN.cv2, h=267, level=0.9)
write.csv(ukmet.ANN.cv2.pred, file = "ukmet ANN cv2.csv")

ukmet.ANN.cv3 <- ets(ukmet.train3, model="ANN")
ukmet.ANN.cv3.pred <- forecast(ukmet.ANN.cv3, h=387, level=0.9)
write.csv(ukmet.ANN.cv3.pred, file = "ukmet ANN cv3.csv")

ukmet.ANN.cv4 <- ets(ukmet.train4, model="ANN")
ukmet.ANN.cv4.pred <- forecast(ukmet.ANN.cv4, h=507, level=0.9)
write.csv(ukmet.ANN.cv4.pred, file = "ukmet ANN cv4.csv")

ukmet.ANN.cv5 <- ets(ukmet.train5, model="ANN")
ukmet.ANN.cv5.pred <- forecast(ukmet.ANN.cv5, h=627, level=0.9)
write.csv(ukmet.ANN.cv5.pred, file = "ukmet ANN cv5.csv")

#tbats model (CV2, CV3, CV4, CV5)

ukmet.tbats.cv2 <- tbats(ukmet.train2)
ukmet.tbats.cv2.pred <- forecast(ukmet.tbats.cv2, h=267, level=0.9)
write.csv(ukmet.tbats.cv2.pred, file = "ukmet TBATS cv2.csv")

ukmet.tbats.cv3 <- tbats(ukmet.train3)
ukmet.tbats.cv3.pred <- forecast(ukmet.tbats.cv3, h=387, level=0.9)
write.csv(ukmet.tbats.cv3.pred, file = "ukmet TBATS cv3.csv")

ukmet.tbats.cv4 <- tbats(ukmet.train4)
ukmet.tbats.cv4.pred <- forecast(ukmet.tbats.cv4, h=507, level=0.9)
write.csv(ukmet.tbats.cv4.pred, file = "ukmet TBATS cv4.csv")

ukmet.tbats.cv5 <- tbats(ukmet.train5)
ukmet.tbats.cv5.pred <- forecast(ukmet.tbats.cv5, h=627, level=0.9)
write.csv(ukmet.tbats.cv5.pred, file = "ukmet TBATS cv5.csv")

#arima model (CV2, CV3, CV4, CV5)
ukmet.arima.cv2 <- auto.arima(ukmet.train2, seasonal=TRUE)
ukmet.arima.cv2.pred <-forecast(ukmet.arima.cv2, h=267, level=0.9)
write.csv(ukmet.arima.cv2.pred, file = "ukmet arima cv2.csv")

ukmet.arima.cv3 <- auto.arima(ukmet.train3, seasonal=TRUE)
ukmet.arima.cv3.pred <-forecast(ukmet.arima.cv3, h=387, level=0.9)
write.csv(ukmet.arima.cv3.pred, file = "ukmet arima cv3.csv")

ukmet.arima.cv4 <- auto.arima(ukmet.train4, seasonal=TRUE)
ukmet.arima.cv4.pred <-forecast(ukmet.arima.cv4, h=507, level=0.9)
write.csv(ukmet.arima.cv4.pred, file = "ukmet arima cv4.csv")

ukmet.arima.cv5 <- auto.arima(ukmet.train5, seasonal=TRUE)
ukmet.arima.cv5.pred <-forecast(ukmet.arima.cv5, h=627, level=0.9)
write.csv(ukmet.arima.cv5.pred, file = "ukmet arima cv5.csv")


#########################
####   FINAL MODEL   ####
#########################

#Since ARIMA tends to better model in all cross validation
#We will now answer Q1 and Q2 using arima model.

#ARIMA model using complete dataset
ukmet.arima.final <- auto.arima(ukmet.ts, seasonal=TRUE)
ukmet.arima.final.pred <-forecast(ukmet.arima.final, h=982, level=0.9)
plot(ukmet.arima.final.pred, xlab="Year", ylab="Global Mean Temperature", ylim=c(11,18), include=500)
checkresiduals(ukmet.arima)
mean(ukmet.arima$residuals)
write.csv(ukmet.arima.final.pred, file = "ukmet arima final pred 2100.csv")

##################################################################\
#Global Warming Revisited(A) UVA-QA-0808

#NASA - Kingston temperature only
##################################################################

#Loading the packages
library(ggplot2)
library(forecast)
library(fpp)
library(fpp2)
library(sqldf)

#Load the datafile

kingston.nasa <- read.csv(file.choose())
head(kingston.nasa)
names(kingston.nasa) <- c("Year", "Month", "temp")
# kingston.nasa$Year <- as.numeric(kingston.nasa$Year)
str(kingston.nasa)

######################
#Construct Time Series
######################
kingston.nasa.ts <- ts(kingston.nasa[,3], frequency=12, start=c(1880,1))
kingston.nasa.train <- window(kingston.nasa.ts, end=c(2006,12))

######################
#Preliminary Analysis
######################
kingston.nasa$decade <- ifelse(kingston.nasa$Year <= 1890, "1880-1890", 
                               ifelse(kingston.nasa$Year <= 1900, "1890-1900",
                                      ifelse(kingston.nasa$Year <= 1910, "1900-1910", 
                                             ifelse(kingston.nasa$Year <= 1920, "1910-1920",
                                                    ifelse(kingston.nasa$Year <= 1930, "1920-1930",
                                                           ifelse(kingston.nasa$Year <= 1940, "1930-1940",
                                                                  ifelse(kingston.nasa$Year <= 1950, "1940-1950",
                                                                         ifelse(kingston.nasa$Year <= 1960, "1950-1960",
                                                                                ifelse(kingston.nasa$Year <= 1970, "1960-1970",
                                                                                       ifelse(kingston.nasa$Year <= 1980, "1970-1980",
                                                                                              ifelse(kingston.nasa$Year <= 1990, "1980-1990",
                                                                                                     ifelse(kingston.nasa$Year <= 2000, "1990-2000",
                                                                                                            ifelse(kingston.nasa$Year <= 2010, "2000-2010",
                                                                                                                   "2010-2019")))))))))))))

kingston.nasa$decade <- as.factor(kingston.nasa$decade)

ggplot(kingston.nasa, aes(x= decade, y=temp, fill = decade)) + geom_boxplot(aes(middle=mean(temp))) + xlab("Decade") + 
  ylab("Average Mean Temperature") + theme_bw() + ggtitle("Average Mean Temperature of Kingston, ON by Decade") + geom_line() + 
  scale_x_discrete(label=abbreviate) + labs(caption ="Datasource: NASA Goddard Institute for Space Studies (GISTEMP)")

autoplot(kingston.nasa.ts) + theme_bw() + ylab("Average Temperature") + xlab("Year") + 
  ggtitle("Kingston, ON Mean Temperature 1880-2019") + labs(caption ="Datasource: NASA Goddard Institute for Space Studies (GISTEMP)")

# ggseasonplot(kingston.nasa.ts)

# ggseasonplot(kingston.nasa.ts, polar=TRUE)

ggsubseriesplot(kingston.nasa.ts) + theme_bw() + xlab("Year") + ylab("Global Temperature") + 
  ggtitle("Sub Series Plot for Kingston, ON Temperature (1880-2019)", sub="Blue line represents mean for the month") + 
  labs(caption ="Datasource: NASA Goddard Institute for Space Studies (GISTEMP)")

gglagplot(kingston.nasa.ts) + ggtitle("Lag Plot of Kingston, ON Mean Temperature (1880-2019)") + xlab("Global Mean Temperature") +
  ylab("Global Mean Temperature") + labs(caption ="Datasource: NASA Goddard Institute for Space Studies (GISTEMP)")

ggAcf(kingston.nasa.ts) + ggtitle("Acf plot for Kingston, ON Mean Temperature (1880-2019)") + theme_bw() + 
  labs(caption ="Datasource: NASA Goddard Institute for Space Studies (GISTEMP)")

ggPacf(kingston.nasa.ts) + ggtitle("Pacf plot for Kingston, ON Mean Temperature (1880-2019)") + theme_bw() + 
  labs(caption ="Datasource: NASA Goddard Institute for Space Studies (GISTEMP)")

##################################################
#Simple Linear Regression Model on ~ time(kingston.nasa.ts)
##################################################
kingston.nasa.ts.lm <- lm(kingston.nasa.ts ~ time(kingston.nasa.ts))
par(mfrow=c(1,4))
plot(kingston.nasa.ts.lm)
# mean(kingston.nasa.ts.lm$residuals)

#Decomposition
decomp.kingston.nasa.add <- decompose(kingston.nasa.ts, type="additive")
decomp.kingston.nasa.mult <- decompose(kingston.nasa.ts, type="multiplicative")
plot(decomp.kingston.nasa.add)
plot(decomp.kingston.nasa.mult)

#Create benchmark models on kingston.nasa.ts3
kingston.nasa.ts2 <- window(kingston.nasa.ts, start=1980) #create benchmark models on 1980 and onwards
autoplot(kingston.nasa.ts2) + theme_bw() + ylab("Average Temperature") + 
  ggtitle("Kingston, ON Mean Temperature and Benchmark Forecasting Models", sub="Actual: 1980 - 2019, Forecast: 2019-2029") + 
  autolayer(meanf(kingston.nasa.ts2, h=120),series="Mean", PI=FALSE) + autolayer(rwf(kingston.nasa.ts2, h=120),series="Naive", PI=FALSE) + 
  autolayer(snaive(kingston.nasa.ts2, h=120),series="Seasonal Naive", PI=FALSE) + xlab("Year") + labs(caption ="Datasource: kingston.nasa Goddard Institute for Space Studies (GISTEMP)")

# Create exponential smoothing models (ETS)
# This will also be used a cross train 1 set 
#(train period 1/1880 - 12/2006, test period 1/2007 - 3/2019)

#First model 'AAA' (Also a cross validate 1 set)
kingston.nasa.AAA <- ets(kingston.nasa.train, model="AAA")
kingston.nasa.AAA.pred <- forecast(kingston.nasa.AAA, h=147, level=0.9)
plot(kingston.nasa.AAA.pred, xlab="Year", ylab="Mean Temperature")
accuracy(kingston.nasa.AAA)
checkresiduals(kingston.nasa.AAA)
mean(kingston.nasa.AAA$residuals)

#CANNOT RUN MULTIPLICATIVE MODEL BECAUSE OF NEGATIVE VALUES
#First model 'MMM' (Also a cross validate 1 set) 
# kingston.nasa.MMM <- ets(kingston.nasa.train, model="MMM")
# kingston.nasa.MMM.pred <- forecast(kingston.nasa.MMM, h=147, level=0.9)
# plot(kingston.nasa.MMM.pred, xlab="Year", ylab="Mean Temperature")
# accuracy(kingston.nasa.MMM)
# checkresiduals(kingston.nasa.MMM)
# mean(kingston.nasa.MMM$residuals)

#First model 'ZZZ' - autotransformed into 'ANA' (Also a cross validate 1 set)
kingston.nasa.ZZZ <- ets(kingston.nasa.train, model="ZZZ")
kingston.nasa.ZZZ.pred <- forecast(kingston.nasa.ZZZ, h=147, level=0.9)
plot(kingston.nasa.ZZZ.pred, xlab="Year", ylab="Mean Temperature")
accuracy(kingston.nasa.ZZZ)
checkresiduals(kingston.nasa.ZZZ)
mean(kingston.nasa.ZZZ$residuals)

# Create a trigonometric box-cox autoregressive trend seasonality (TBATS) model (Also a cross validate 1 set)
kingston.nasa.tbats <- tbats(kingston.nasa.train)
kingston.nasa.tbats.pred <- forecast(kingston.nasa.tbats, h=147, level=0.9)
plot(kingston.nasa.tbats.pred, xlab="Year", ylab="Mean Temperature")
accuracy(kingston.nasa.tbats)
checkresiduals(kingston.nasa.tbats)
mean(kingston.nasa.tbats$errors)

#ARIMA model (Also a cross validate 1 set)
kingston.nasa.arima <- auto.arima(kingston.nasa.train, seasonal=TRUE)
kingston.nasa.arima.pred <-forecast(kingston.nasa.arima, h=147, level=0.9)
plot(kingston.nasa.arima.pred, xlab="Year", ylab="Mean Temperature")
accuracy(kingston.nasa.arima)
checkresiduals(kingston.nasa.arima)
mean(kingston.nasa.arima$residuals)

par(mfrow=c(1,4)) # Lets look at the three models with seasonality on one graph on the same scale
plot(kingston.nasa.AAA.pred, xlab="Year", ylab="Global Mean Temperature", ylim=c(-15,30), include=500)
# plot(kingston.nasa.MMM.pred, xlab="Year", ylab="Global Mean Temperature", ylim=c(0,20), include=500)
plot(kingston.nasa.ZZZ.pred, xlab="Year", ylab="Global Mean Temperature", ylim=c(-15,30), include=500) #ANA
plot(kingston.nasa.tbats.pred, xlab="Year", ylab="Global Mean Temperature", ylim=c(-15,30), include=500)
plot(kingston.nasa.arima.pred, xlab="Year", ylab="Global Mean Temperature", ylim=c(-15,30), include=500)

#write models to csv
write.csv(kingston.nasa.AAA.pred, file = "kingston.nasa AAA Forecast.csv")
# write.csv(kingston.nasa.MMM.pred, file = "kingston.nasa MMM Forecast.csv")
write.csv(kingston.nasa.ZZZ.pred, file = "kingston.nasa ANA Forecast.csv")
write.csv(kingston.nasa.tbats.pred, file = "kingston.nasa TBATS Forecast.csv")
write.csv(kingston.nasa.arima.pred, file = "kingston.nasa ARIMA Forecast.csv")

#Cross Validation
#First model built above is CV1
kingston.nasa.train2 <- window(kingston.nasa.ts, end=c(1996,12))
kingston.nasa.train3 <- window(kingston.nasa.ts, end=c(1986,12))
kingston.nasa.train4 <- window(kingston.nasa.ts, end=c(1976,12))
kingston.nasa.train5 <- window(kingston.nasa.ts, end=c(1966,12))

#AAA Models (CV2, CV3, CV4, CV5)

kingston.nasa.AAA.cv2 <- ets(kingston.nasa.train2, model="AAA")
kingston.nasa.AAA.cv2.pred <- forecast(kingston.nasa.AAA.cv2, h=267, level=0.9)
write.csv(kingston.nasa.AAA.cv2.pred, file = "kingston.nasa AAA cv2.csv")

kingston.nasa.AAA.cv3 <- ets(kingston.nasa.train3, model="AAA")
kingston.nasa.AAA.cv3.pred <- forecast(kingston.nasa.AAA.cv3, h=387, level=0.9)
write.csv(kingston.nasa.AAA.cv3.pred, file = "kingston.nasa AAA cv3.csv")

kingston.nasa.AAA.cv4 <- ets(kingston.nasa.train4, model="AAA")
kingston.nasa.AAA.cv4.pred <- forecast(kingston.nasa.AAA.cv4, h=507, level=0.9)
write.csv(kingston.nasa.AAA.cv4.pred, file = "kingston.nasa AAA cv4.csv")

kingston.nasa.AAA.cv5 <- ets(kingston.nasa.train5, model="AAA")
kingston.nasa.AAA.cv5.pred <- forecast(kingston.nasa.AAA.cv5, h=627, level=0.9)
write.csv(kingston.nasa.AAA.cv5.pred, file = "kingston.nasa AAA cv5.csv")

#ANA Models (CV2, CV3, CV4, CV5)

kingston.nasa.ANA.cv2 <- ets(kingston.nasa.train2, model="ANA")
kingston.nasa.ANA.cv2.pred <- forecast(kingston.nasa.ANA.cv2, h=267, level=0.9)
write.csv(kingston.nasa.ANA.cv2.pred, file = "kingston.nasa ANA cv2.csv")

kingston.nasa.ANA.cv3 <- ets(kingston.nasa.train3, model="ANA")
kingston.nasa.ANA.cv3.pred <- forecast(kingston.nasa.ANA.cv3, h=387, level=0.9)
write.csv(kingston.nasa.ANA.cv3.pred, file = "kingston.nasa ANA cv3.csv")

kingston.nasa.ANA.cv4 <- ets(kingston.nasa.train4, model="ANA")
kingston.nasa.ANA.cv4.pred <- forecast(kingston.nasa.ANA.cv4, h=507, level=0.9)
write.csv(kingston.nasa.ANA.cv4.pred, file = "kingston.nasa ANA cv4.csv")

kingston.nasa.ANA.cv5 <- ets(kingston.nasa.train5, model="ANA")
kingston.nasa.ANA.cv5.pred <- forecast(kingston.nasa.ANA.cv5, h=627, level=0.9)
write.csv(kingston.nasa.ANA.cv5.pred, file = "kingston.nasa ANA cv5.csv")

#tbats model (CV2, CV3, CV4, CV5)

kingston.nasa.tbats.cv2 <- tbats(kingston.nasa.train2)
kingston.nasa.tbats.cv2.pred <- forecast(kingston.nasa.tbats.cv2, h=267, level=0.9)
write.csv(kingston.nasa.tbats.cv2.pred, file = "kingston.nasa TBATS cv2.csv")

kingston.nasa.tbats.cv3 <- tbats(kingston.nasa.train3)
kingston.nasa.tbats.cv3.pred <- forecast(kingston.nasa.tbats.cv3, h=387, level=0.9)
write.csv(kingston.nasa.tbats.cv3.pred, file = "kingston.nasa TBATS cv3.csv")

kingston.nasa.tbats.cv4 <- tbats(kingston.nasa.train4)
kingston.nasa.tbats.cv4.pred <- forecast(kingston.nasa.tbats.cv4, h=507, level=0.9)
write.csv(kingston.nasa.tbats.cv4.pred, file = "kingston.nasa TBATS cv4.csv")

kingston.nasa.tbats.cv5 <- tbats(kingston.nasa.train5)
kingston.nasa.tbats.cv5.pred <- forecast(kingston.nasa.tbats.cv5, h=627, level=0.9)
write.csv(kingston.nasa.tbats.cv5.pred, file = "kingston.nasa TBATS cv5.csv")

#arima model (CV2, CV3, CV4, CV5)
kingston.nasa.arima.cv2 <- auto.arima(kingston.nasa.train2, seasonal=TRUE)
kingston.nasa.arima.cv2.pred <-forecast(kingston.nasa.arima.cv2, h=267, level=0.9)
write.csv(kingston.nasa.arima.cv2.pred, file = "kingston.nasa arima cv2.csv")

kingston.nasa.arima.cv3 <- auto.arima(kingston.nasa.train3, seasonal=TRUE)
kingston.nasa.arima.cv3.pred <-forecast(kingston.nasa.arima.cv3, h=387, level=0.9)
write.csv(kingston.nasa.arima.cv3.pred, file = "kingston.nasa arima cv3.csv")

kingston.nasa.arima.cv4 <- auto.arima(kingston.nasa.train4, seasonal=TRUE)
kingston.nasa.arima.cv4.pred <-forecast(kingston.nasa.arima.cv4, h=507, level=0.9)
write.csv(kingston.nasa.arima.cv4.pred, file = "kingston.nasa arima cv4.csv")

kingston.nasa.arima.cv5 <- auto.arima(kingston.nasa.train5, seasonal=TRUE)
kingston.nasa.arima.cv5.pred <-forecast(kingston.nasa.arima.cv5, h=627, level=0.9)
write.csv(kingston.nasa.arima.cv5.pred, file = "kingston.nasa arima cv5.csv")


#########################
####   FINAL MODEL   ####
#########################

#Since TBATS tends to better model in all cross validation based on long-range forecasts
#We will now answer Q3 using TBATS model.

#TBATS model

kingston.nasa.tbats.final <- tbats(kingston.nasa.ts)
kingston.nasa.tbats.final.pred <- forecast(kingston.nasa.tbats.final, h=981, level=0.9)
plot(kingston.nasa.tbats.final.pred, xlab="Year", ylab="Mean Temperature")
accuracy(kingston.nasa.tbats.final)
checkresiduals(kingston.nasa.tbats.final)
mean(kingston.nasa.tbats$errors)
write.csv(kingston.nasa.tbats.final.pred, file = "Kingston NASA TBATS Final Pred 2100.csv")

##################################################################
#Global Warming Revisited(A) UVA-QA-0808

#UKMET - Kingston temperature only
##################################################################

#Loading the packages
library(ggplot2)
library(forecast)
library(fpp)
library(fpp2)
library(sqldf)

#Load the datafile

kingston.ukmet <- read.csv(file.choose())
head(kingston.ukmet)
names(kingston.ukmet) <- c("Year", "Month", "temp")
# kingston.ukmet$Year <- as.numeric(kingston.ukmet$Year)
kingston.ukmet <- kingston.ukmet[kingston.ukmet$Year >= 1880, ]
str(kingston.ukmet)

######################
#Construct Time Series
######################
kingston.ukmet.ts <- ts(kingston.ukmet[,3], frequency=12, start=c(1880,1))
kingston.ukmet.train <- window(kingston.ukmet.ts, end=c(2006,12))
str(kingston.ukmet.ts)

######################
#Preliminary Analysis
######################
kingston.ukmet$decade <- ifelse(kingston.ukmet$Year <= 1890, "1880-1890", 
                                ifelse(kingston.ukmet$Year <= 1900, "1890-1900",
                                       ifelse(kingston.ukmet$Year <= 1910, "1900-1910", 
                                              ifelse(kingston.ukmet$Year <= 1920, "1910-1920",
                                                     ifelse(kingston.ukmet$Year <= 1930, "1920-1930",
                                                            ifelse(kingston.ukmet$Year <= 1940, "1930-1940",
                                                                   ifelse(kingston.ukmet$Year <= 1950, "1940-1950",
                                                                          ifelse(kingston.ukmet$Year <= 1960, "1950-1960",
                                                                                 ifelse(kingston.ukmet$Year <= 1970, "1960-1970",
                                                                                        ifelse(kingston.ukmet$Year <= 1980, "1970-1980",
                                                                                               ifelse(kingston.ukmet$Year <= 1990, "1980-1990",
                                                                                                      ifelse(kingston.ukmet$Year <= 2000, "1990-2000",
                                                                                                             ifelse(kingston.ukmet$Year <= 2010, "2000-2010",
                                                                                                                    "2010-2019")))))))))))))

kingston.ukmet$decade <- as.factor(kingston.ukmet$decade)

ggplot(kingston.ukmet, aes(x= decade, y=temp, fill = decade)) + geom_boxplot(aes(middle=mean(temp))) + xlab("Decade") + 
  ylab("Average Mean Temperature") + theme_bw() + ggtitle("Average Mean Temperature of Kingston, ON by Decade") + geom_line() + 
  scale_x_discrete(label=abbreviate) + labs(caption ="Datasource: UK Met Office (HadCRUT4)")

autoplot(kingston.ukmet.ts) + theme_bw() + ylab("Average Temperature") + xlab("Year") + 
  ggtitle("Kingston, ON Mean Temperature 1880-2019") + labs(caption ="Datasource: UK Met Office (HadCRUT4)")

# ggseasonplot(kingston.ukmet.ts)

# ggseasonplot(kingston.ukmet.ts, polar=TRUE)

ggsubseriesplot(kingston.ukmet.ts) + theme_bw() + xlab("Year") + ylab("Global Temperature") + 
  ggtitle("Sub Series Plot for Kingston, ON Temperature (1880-2019)", sub="Blue line represents mean for the month") + 
  labs(caption ="Datasource: UK Met Office (HadCRUT4)")

gglagplot(kingston.ukmet.ts) + ggtitle("Lag Plot of Kingston, ON Mean Temperature (1880-2019)") + xlab("Global Mean Temperature") +
  ylab("Global Mean Temperature") + labs(caption ="Datasource: UK Met Office (HadCRUT4)")

ggAcf(kingston.ukmet.ts) + ggtitle("Acf plot for Kingston, ON Mean Temperature (1880-2019)") + theme_bw() + 
  labs(caption ="Datasource: UK Met Office (HadCRUT4)")

ggPacf(kingston.ukmet.ts) + ggtitle("Pacf plot for Kingston, ON Mean Temperature (1880-2019)") + theme_bw() + 
  labs(caption ="Datasource: UK Met Office (HadCRUT4)")

##################################################
#Simple Linear Regression Model on ~ time(kingston.ukmet.ts)
##################################################
kingston.ukmet.ts.lm <- lm(kingston.ukmet.ts ~ time(kingston.ukmet.ts))
par(mfrow=c(1,4))
plot(kingston.ukmet.ts.lm)
# mean(kingston.ukmet.ts.lm$residuals)

#Decomposition
decomp.kingston.ukmet.add <- decompose(kingston.ukmet.ts, type="additive")
decomp.kingston.ukmet.mult <- decompose(kingston.ukmet.ts, type="multiplicative")
plot(decomp.kingston.ukmet.add)
plot(decomp.kingston.ukmet.mult)

#Create benchmark models on kingston.ukmet.ts3
kingston.ukmet.ts2 <- window(kingston.ukmet.ts, start=1980) #create benchmark models on 1980 and onwards
autoplot(kingston.ukmet.ts2) + theme_bw() + ylab("Average Temperature") + 
  ggtitle("Kingston, ON Mean Temperature and Benchmark Forecasting Models", sub="Actual: 1980 - 2019, Forecast: 2019-2029") + 
  autolayer(meanf(kingston.ukmet.ts2, h=120),series="Mean", PI=FALSE) + autolayer(rwf(kingston.ukmet.ts2, h=120),series="Naive", PI=FALSE) + 
  autolayer(snaive(kingston.ukmet.ts2, h=120),series="Seasonal Naive", PI=FALSE) + xlab("Year") + labs(caption ="Datasource: UK Met Office (HadCRUT4)")

# Create exponential smoothing models (ETS)
# This will also be used a cross train 1 set 
#(train period 1/1880 - 12/2006, test period 1/2007 - 3/2019)

#First model 'AAA' (Also a cross validate 1 set)
kingston.ukmet.AAA <- ets(kingston.ukmet.train, model="AAA")
kingston.ukmet.AAA.pred <- forecast(kingston.ukmet.AAA, h=147, level=0.9)
plot(kingston.ukmet.AAA.pred, xlab="Year", ylab="Mean Temperature")
accuracy(kingston.ukmet.AAA)
checkresiduals(kingston.ukmet.AAA)
mean(kingston.ukmet.AAA$residuals)

#CANNOT RUN MULTIPLICATIVE MODEL BECAUSE OF NEGATIVE VALUES
#First model 'MMM' (Also a cross validate 1 set) 
# kingston.ukmet.MMM <- ets(kingston.ukmet.train, model="MMM")
# kingston.ukmet.MMM.pred <- forecast(kingston.ukmet.MMM, h=147, level=0.9)
# plot(kingston.ukmet.MMM.pred, xlab="Year", ylab="Mean Temperature")
# accuracy(kingston.ukmet.MMM)
# checkresiduals(kingston.ukmet.MMM)
# mean(kingston.ukmet.MMM$residuals)

#First model 'ANA'
kingston.ukmet.ANA <- ets(kingston.ukmet.train, model="ANA")
kingston.ukmet.ANA.pred <- forecast(kingston.ukmet.ANA, h=147, level=0.9)
plot(kingston.ukmet.ANA.pred, xlab="Year", ylab="Mean Temperature")
accuracy(kingston.ukmet.ZZZ)
checkresiduals(kingston.ukmet.ZZZ)
mean(kingston.ukmet.ZZZ$residuals)


# Create a trigonometric box-cox autoregressive trend seasonality (TBATS) model (Also a cross validate 1 set)
kingston.ukmet.tbats <- tbats(kingston.ukmet.train)
kingston.ukmet.tbats.pred <- forecast(kingston.ukmet.tbats, h=147, level=0.9)
plot(kingston.ukmet.tbats.pred, xlab="Year", ylab="Mean Temperature")
accuracy(kingston.ukmet.tbats)
checkresiduals(kingston.ukmet.tbats)
mean(kingston.ukmet.tbats$errors)

#ARIMA model (Also a cross validate 1 set)
kingston.ukmet.arima <- auto.arima(kingston.ukmet.train, seasonal=TRUE)
kingston.ukmet.arima.pred <-forecast(kingston.ukmet.arima, h=147, level=0.9)
plot(kingston.ukmet.arima.pred, xlab="Year", ylab="Mean Temperature")
accuracy(kingston.ukmet.arima)
checkresiduals(kingston.ukmet.arima)
mean(kingston.ukmet.arima$residuals)

par(mfrow=c(1,4)) # Lets look at the three models with seasonality on one graph on the same scale
plot(kingston.ukmet.AAA.pred, xlab="Year", ylab="Global Mean Temperature", ylim=c(-15,30), include=500)
# plot(kingston.ukmet.MMM.pred, xlab="Year", ylab="Global Mean Temperature", ylim=c(0,20), include=500)
plot(kingston.ukmet.ANA.pred, xlab="Year", ylab="Global Mean Temperature", ylim=c(-15,30), include=500) #ANA
plot(kingston.ukmet.tbats.pred, xlab="Year", ylab="Global Mean Temperature", ylim=c(-15,30), include=500)
plot(kingston.ukmet.arima.pred, xlab="Year", ylab="Global Mean Temperature", ylim=c(-15,30), include=500)

#write models to csv
write.csv(kingston.ukmet.AAA.pred, file = "kingston.ukmet AAA Forecast.csv")
# write.csv(kingston.ukmet.MMM.pred, file = "kingston.ukmet MMM Forecast.csv")
write.csv(kingston.ukmet.ANA.pred, file = "kingston.ukmet ANA Forecast.csv")
write.csv(kingston.ukmet.tbats.pred, file = "kingston.ukmet TBATS Forecast.csv")
write.csv(kingston.ukmet.arima.pred, file = "kingston.ukmet ARIMA Forecast.csv")

#Cross Validation
#First model built above is CV1
kingston.ukmet.train2 <- window(kingston.ukmet.ts, end=c(1996,12))
kingston.ukmet.train3 <- window(kingston.ukmet.ts, end=c(1986,12))
kingston.ukmet.train4 <- window(kingston.ukmet.ts, end=c(1976,12))
kingston.ukmet.train5 <- window(kingston.ukmet.ts, end=c(1966,12))

#AAA Models (CV2, CV3, CV4, CV5)

kingston.ukmet.AAA.cv2 <- ets(kingston.ukmet.train2, model="AAA")
kingston.ukmet.AAA.cv2.pred <- forecast(kingston.ukmet.AAA.cv2, h=267, level=0.9)
write.csv(kingston.ukmet.AAA.cv2.pred, file = "kingston.ukmet AAA cv2.csv")

kingston.ukmet.AAA.cv3 <- ets(kingston.ukmet.train3, model="AAA")
kingston.ukmet.AAA.cv3.pred <- forecast(kingston.ukmet.AAA.cv3, h=387, level=0.9)
write.csv(kingston.ukmet.AAA.cv3.pred, file = "kingston.ukmet AAA cv3.csv")

kingston.ukmet.AAA.cv4 <- ets(kingston.ukmet.train4, model="AAA")
kingston.ukmet.AAA.cv4.pred <- forecast(kingston.ukmet.AAA.cv4, h=507, level=0.9)
write.csv(kingston.ukmet.AAA.cv4.pred, file = "kingston.ukmet AAA cv4.csv")

kingston.ukmet.AAA.cv5 <- ets(kingston.ukmet.train5, model="AAA")
kingston.ukmet.AAA.cv5.pred <- forecast(kingston.ukmet.AAA.cv5, h=627, level=0.9)
write.csv(kingston.ukmet.AAA.cv5.pred, file = "kingston.ukmet AAA cv5.csv")

#ANA Models (CV2, CV3, CV4, CV5)

kingston.ukmet.ANA.cv2 <- ets(kingston.ukmet.train2, model="ANA")
kingston.ukmet.ANA.cv2.pred <- forecast(kingston.ukmet.ANA.cv2, h=267, level=0.9)
write.csv(kingston.ukmet.ANA.cv2.pred, file = "kingston.ukmet ANA cv2.csv")

kingston.ukmet.ANA.cv3 <- ets(kingston.ukmet.train3, model="ANA")
kingston.ukmet.ANA.cv3.pred <- forecast(kingston.ukmet.ANA.cv3, h=387, level=0.9)
write.csv(kingston.ukmet.ANA.cv3.pred, file = "kingston.ukmet ANA cv3.csv")

kingston.ukmet.ANA.cv4 <- ets(kingston.ukmet.train4, model="ANA")
kingston.ukmet.ANA.cv4.pred <- forecast(kingston.ukmet.ANA.cv4, h=507, level=0.9)
write.csv(kingston.ukmet.ANA.cv4.pred, file = "kingston.ukmet ANA cv4.csv")

kingston.ukmet.ANA.cv5 <- ets(kingston.ukmet.train5, model="ANA")
kingston.ukmet.ANA.cv5.pred <- forecast(kingston.ukmet.ANA.cv5, h=627, level=0.9)
write.csv(kingston.ukmet.ANA.cv5.pred, file = "kingston.ukmet ANA cv5.csv")

#tbats model (CV2, CV3, CV4, CV5)

kingston.ukmet.tbats.cv2 <- tbats(kingston.ukmet.train2)
kingston.ukmet.tbats.cv2.pred <- forecast(kingston.ukmet.tbats.cv2, h=267, level=0.9)
write.csv(kingston.ukmet.tbats.cv2.pred, file = "kingston.ukmet TBATS cv2.csv")

kingston.ukmet.tbats.cv3 <- tbats(kingston.ukmet.train3)
kingston.ukmet.tbats.cv3.pred <- forecast(kingston.ukmet.tbats.cv3, h=387, level=0.9)
write.csv(kingston.ukmet.tbats.cv3.pred, file = "kingston.ukmet TBATS cv3.csv")

kingston.ukmet.tbats.cv4 <- tbats(kingston.ukmet.train4)
kingston.ukmet.tbats.cv4.pred <- forecast(kingston.ukmet.tbats.cv4, h=507, level=0.9)
write.csv(kingston.ukmet.tbats.cv4.pred, file = "kingston.ukmet TBATS cv4.csv")

kingston.ukmet.tbats.cv5 <- tbats(kingston.ukmet.train5)
kingston.ukmet.tbats.cv5.pred <- forecast(kingston.ukmet.tbats.cv5, h=627, level=0.9)
write.csv(kingston.ukmet.tbats.cv5.pred, file = "kingston.ukmet TBATS cv5.csv")

#arima model (CV2, CV3, CV4, CV5)
kingston.ukmet.arima.cv2 <- auto.arima(kingston.ukmet.train2, seasonal=TRUE)
kingston.ukmet.arima.cv2.pred <-forecast(kingston.ukmet.arima.cv2, h=267, level=0.9)
write.csv(kingston.ukmet.arima.cv2.pred, file = "kingston.ukmet arima cv2.csv")

kingston.ukmet.arima.cv3 <- auto.arima(kingston.ukmet.train3, seasonal=TRUE)
kingston.ukmet.arima.cv3.pred <-forecast(kingston.ukmet.arima.cv3, h=387, level=0.9)
write.csv(kingston.ukmet.arima.cv3.pred, file = "kingston.ukmet arima cv3.csv")

kingston.ukmet.arima.cv4 <- auto.arima(kingston.ukmet.train4, seasonal=TRUE)
kingston.ukmet.arima.cv4.pred <-forecast(kingston.ukmet.arima.cv4, h=507, level=0.9)
write.csv(kingston.ukmet.arima.cv4.pred, file = "kingston.ukmet arima cv4.csv")

kingston.ukmet.arima.cv5 <- auto.arima(kingston.ukmet.train5, seasonal=TRUE)
kingston.ukmet.arima.cv5.pred <-forecast(kingston.ukmet.arima.cv5, h=627, level=0.9)
write.csv(kingston.ukmet.arima.cv5.pred, file = "kingston.ukmet arima cv5.csv")


#########################
####   FINAL MODEL   ####
#########################

#Since TBATS tends to better model in all cross validation based on long-range forecasts
#We will now answer Q3 using TBATS model.

#TBATS model

kingston.ukmet.tbats.final <- tbats(kingston.ukmet.ts)
kingston.ukmet.tbats.final.pred <- forecast(kingston.ukmet.tbats.final, h=981, level=0.9)
plot(kingston.ukmet.tbats.final.pred, xlab="Year", ylab="Mean Temperature")
accuracy(kingston.ukmet.tbats.final)
checkresiduals(kingston.ukmet.tbats.final)
mean(kingston.ukmet.tbats$errors)
write.csv(kingston.ukmet.tbats.final.pred, file = "Kingston UKMET TBATS Final Pred 2100.csv")

str(kingston.ukmet.ts)

#Co2 Emission Data (External Dataset to be used as regressor)
#1958 - to date
#https://www.co2.earth/monthly-co2#noaa


#Importing Data
co2 <- read.csv(file.choose(), stringsAsFactors = FALSE)
head(co2)

#Constructing time series
co2.ts <- ts(co2[,3], frequency = 12, start=1958)
co2.ts
str(co2.ts)

#Preliminary Analysis
autoplot(co2.ts) + theme_bw() + ylab("Monthly mean concentration (ppm)") + xlab("Year") + 
  ggtitle("Monthly Co2 Mean Concentration") + labs(caption ="Datasource: https://www.co2.earth/monthly-co2")

decomp.co2.add <- decompose(co2.ts)
plot(decomp.co2.add)

ggseasonplot(co2.ts, polar=TRUE) + ggtitle("Season Plot for Monthly Mean Concentration of Co2 (ppm)") + theme_bw() + 
  ylab("Monthly mean concentration (ppm)") + labs(caption ="Datasource: https://www.co2.earth/monthly-co2")


#Building Arima Model to predict future co2 level
co2.fit <- auto.arima(co2.ts, seasonal = TRUE)
co2.fit.pred <- forecast(co2.fit, h=981, level=0.9)
autoplot(co2.fit.pred) + ggtitle("") + theme_bw() + ggtitle("Forecast of Co2 Emission Level through 2100") + xlab("Year") + ylab("Co2 Emission (ppm)")
checkresiduals(co2.fit)
mean(co2.fit$residuals)
write.csv(co2.fit.pred, "co2 prediction through 2100.csv") #write csv

co2.future.matrix <- as.matrix(co2.fit.pred$mean)

#Importing NASA weather data and subsetting from 1958 onwards

nasa <- read.csv(file.choose(), stringsAsFactors = FALSE)
nasa$temp <- 14 + nasa$Anomaly
names(nasa) <- c("Year", "Month", "Anomaly", "temp")
nasa <- nasa[nasa$Year >= 1958,]
nasa <- nasa[,1>=1958]
head(nasa)

#Construct a time series
nasa.ts <- ts(nasa[,4], frequency=12, start=1958)
nasa.ts

#Build a arima model using co2 as regressor (train = 1958 through 2019)
temp.fit <- auto.arima(nasa.ts, xreg=co2$Co2)
temp.fit.predict <- forecast(temp.fit, xreg=co2.future.matrix, h=981)
plot(temp.fit.predict)
checkresiduals(temp.fit)
mean(temp.fit$residuals)
write.csv(temp.fit.predict, "forecast using co2.csv")


visual <- read.csv(file.choose(), stringsAsFactors =FALSE)
head(visual)
ggplot(visual, aes(x=Co2, y=Temp)) + geom_point(color="blue", alpha=0.5) + geom_smooth(method='lm', se=FALSE, color = "red") + theme_bw() + 
  xlab("Monthly mean concentration (ppm)") + ylab("Global Mean Temperature") + ggtitle("Relationship between Co2 Emission and Temperature")