library(readxl)
library(sqldf)
library(dplyr)
library(tidyr)
library(caret)
library(magrittr)
library(stringr)
library(tidyverse)
library(taRifx)
library(ggthemes)
library(corrplot)

getwd()

weather <- read_excel("weather.xlsx")
head(weather)
str(weather)
View(weather)
names(weather)
new_col <- c("Date", "Year", "Month", "Day", "Data Quality", "Max Temp", "Max Temp Flag", "Min Temp", "Min Temp Flag",
             "Mean Temp", "Mean Temp Flag", "Heat Deg Days", "Heat Deg Days Flag", "Cool Deg Days", "Cool Deg Days Flag",
             "Total Rain", "Total Rain Flag", "Total Snow", "Total Snow Flag", "Total Precip", "Total Precip Flag",
             "Snow on Ground", "Snow on Ground Flag", "Dir of Max Gust", "Dir of Max Gust Flag", "Spd of Max Gust",
             "Spd of Max Gust Flag")

compare_headers <- data.frame(names(weather), new_col)
View(compare_headers) #to check if the renaming is done correctly

str(weather)
names(weather) <- new_col
head(weather) # to check if renaming is done

names(weather)

#using sqldf select specific columns that are needed

weather <- sqldf('SELECT Date, "Year", "Month", "Day", "Mean Temp", "Total Rain", "Total Precip", "Total Snow" FROM weather')
weather$Date <- as.Date(weather$Date)
head(weather)
View(weather)

# add week
week_list <- data.frame((rep(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
                               21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,
                               38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,
                               63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
                               81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,
                               101,102,103,104), times = c(7))))

str(week_list)
View(week_list)
names(week_list) <- c("Week")
head(week_list)
nrow(week_list) # should be 728 rows



week_list <- 
  week_list %>% 
  arrange(Week)

View(week_list)

weather_weekly <- cbind(weather, week_list)
View(weather_weekly)

#using sql df summarize the data on week basis
head(weather_weekly)
names(weather_weekly)
weather_weekly <- sqldf('SELECT Week, avg("Mean Temp") as "AverageTemp", sum("Total Rain") as "TotalRain", sum("Total Snow") as "TotalSnow", sum("Total Precip") as "TotalPrecip" FROM weather_weekly GROUP BY Week ORDER BY Week ASC')
View(weather_weekly)
str(weather_weekly)

weather_weekly$Rainflag <- ifelse(weather_weekly$TotalRain > 0, 1, 0)
weather_weekly$Snowflag <- ifelse(weather_weekly$TotalSnow > 0, 1, 0)
weather_weekly$Precipflag <- ifelse(weather_weekly$TotalPrecip > 0, 1, 0)

View(weather_weekly)
#FINAL data.frame TO BE USED FOR JOIN IS weather_weekly. JOIN ON BASIS OF col $ week



####
####Setup coffee data
####
library(Rserve)
Rserve()

coffee <- read_excel("coffee.xlsx", sheet = "Sheet1")

##Technique 1: SQL DATA QUERIES##

#Add Underscores to column names
names(coffee)
names(coffee) <- gsub(" ", "_", names(coffee))

#Add week reference column
coffee$Week <- if_else(coffee$Year == "LY", coffee$Last_Week_ASC, (coffee$Last_Week_ASC + 52))
unique(coffee$Week)

#convert columns to numeric
coffee[,11:73] <- sapply(coffee[,11:73], as.numeric)

#Remove whitespace from week ending
coffee$Week_Ending <- str_replace_all(coffee$Week_Ending, fixed(" "), "")
view(coffee)

str(coffee)

#Join coffee and weather data
coffee_join <- left_join(coffee, weather_weekly, by = "Week")
View(coffee_join)
str(coffee_joind)

write.csv(coffee_joindata, file = "coffeejoin.csv")


#SQL Exploration

#1
sqldf('SELECT COUNT(*), COUNT(DISTINCT Article), SUM(Sales_Vol), AVG(Sales_Vol) FROM coffee_join')

#2
sqldf('SELECT SUM(Unit_Vol), AVG(Unit_Vol), SUM(TPR_Unit_Vol) / SUM(Unit_Vol) FROM coffee_join')

#3
sqldf('SELECT COUNT(Article), Sales_Vol FROM coffee_join 
      where Sales_Vol = (select min(Sales_vol) from coffee_join)')

sqldf('SELECT COUNT(Article), Sales_Vol FROM coffee_join 
      where Sales_Vol = (select max(Sales_vol) from coffee_join)')


#4
sqldf('SELECT Week_Ending, SUM(Sales_Vol), SUM(UNIT_Vol), SUM(Tonn_Vol) 
      FROM coffee_join 
      GROUP BY Week_Ending 
      ORDER BY week DESC')


#5
sqldf('SELECT SEGMENT, SUM(Sales_Vol), AVG(Sales_Vol), COUNT(DISTINCT ARTICLE)  
      FROM coffee_join 
      GROUP BY SEGMENT
      ORDER BY SUM(Sales_Vol) DESC')

#6
sqldf('SELECT BRAND, SUM(Sales_Vol), AVG(Sales_Vol), SUM(TPR_Sales_Vol) / SUM(Sales_Vol), AVG(Unit_Vol), AVG(Avg_Retail_Unit_Price)
      FROM coffee
      GROUP BY BRAND
      ORDER BY SUM(Sales_Vol) DESC
      LIMIT 10')

#7
sqldf('SELECT ARTICLE, SEGMENT, SUM(Sales_Vol), AVG(Sales_Vol), AVG(Sales_Cat_Shr), AVG(Sales_Mkt_Imp)
      FROM coffee
      GROUP BY BRAND
      ORDER BY SUM(Sales_Vol) DESC
      LIMIT 10')

#8
sqldf('SELECT MANUFACTURER, AVG(Avg_AC_Dist), AVG(Sales_SPPD)
      FROM coffee
      WHERE SEGMENT = "SINGLE SERVE COFFEE"
      GROUP BY MANUFACTURER
      ORDER BY AVG(Avg_AC_Dist) DESC
      LIMIT 10')




sqldf('SELECT brand, COUNT(DISTINCT Article), SEGMENT FROM coffee WHERE BRAND = "STARBUCKS"')

sqldf('SELECT COUNT(*) FROM coffee WHERE Sales_Vol < 2')
sqldf('SELECT COUNT(*) FROM coffee WHERE Sales_Vol < 10')
sqldf('SELECT COUNT(*) FROM coffee WHERE Sales_Vol < 100')
sqldf('SELECT COUNT(*) FROM coffee WHERE Total_Distribution_Points == 0')
####################

#1
str(coffee_join)

#2
summary(coffee_join)



#3
cor(reg_data_1$TotalPrecip, reg_data_1$AverageTemp)
cor(reg_data_1$Sales, reg_data_1$TotalPrecip)

coffeecor <- coffee_join[,13:80]
dplyr_if_else <- function(x) { mutate_all(x, ~if_else(is.na(.), 0, .)) }
coffeecor1 <- dplyr_if_else(coffeecor)

corrplot(cor(coffeecor2), diag = FALSE, order = "FPC", 
         tl.pos = "td", tl.cex = 0.5, method = "color", type = "upper")

coffeecor2 <- na.omit(coffeecor)

#4
ggplot(coffee, aes(x=coffee_join$Sales_Vol)) + geom_histogram(binwidth = 5000) + theme_economist() + ggtitle("Sales Histogram")
ggplot(coffee, aes(x=coffee_join$Avg_Retail_Unit_Price)) + geom_histogram(binwidth = 1) + theme_economist() + xlim(0, 100) + ggtitle("AVG Price Histogram")
ggplot(coffee, aes(x=coffee_join$Sales_SPPD,)) + geom_histogram(binwidth = 1) + theme_economist() + xlim(0, 2000) + ggtitle("Sales Per Point of Distribution Histogram")

ggplot(coffee, aes(x=coffee_join$Sales_Mkt_Imp)) + geom_histogram(binwidth = 1) + theme_economist()
ggplot(coffee, aes(x=coffee_join$Avg_AC_Dist)) + geom_histogram(binwidth = 1) + theme_economist() 

#5
# SEGMENT ANALYSIS

coffee.segment <- sqldf('SELECT SEGMENT as Segment, Sales_Vol as Sales FROM coffee_join')
coffee.segment

ggplot(coffee.segment, aes(x=Segment, y=Sales)) + geom_boxplot() + theme_economist()+ ggtitle("Segment Analysis") + 
  labs(x="Segment", y="Sales $") + theme(text = element_text(size=8), axis.text.x = element_text(angle=90, hjust=1)) + geom_smooth(method="lm") + ylim(0, 50000)

#6
#BRAND ANALYSIS

coffee.brand <- sqldf('SELECT BRAND as Brand, Sales_Vol as Sales FROM coffee_join')
coffee.brandtop10 <- sqldf('SELECT BRAND, sum(Sales_Vol) as Sales FROM coffee_join GROUP BY BRAND')
coffee.brandtop10

coffee.brandtop10.names <-
  coffee.brandtop10 %>% 
  arrange(desc(Sales)) %>% 
  top_n(10) %>% 
  select(BRAND)

names(coffee.brandtop10.names) <- "Brand"
coffee.brandtop10.names

coffee.top10.sales <- 
  coffee.brand %>% 
  filter(coffee.brand$Brand %in% coffee.brandtop10.names$Brand)

ggplot(coffee.top10.sales, aes(x=Brand, y=Sales,)) + geom_boxplot() + theme_economist()+
  ggtitle("Top 10 Coffee Brands") + labs(x="Brand", y="Sales$")+ theme(text = element_text(size=8))



#7
#Article ANALYSIS
coffee.article <- sqldf('SELECT Article as Article, Sales_Vol as Sales FROM coffee_join')
coffee.articletop10 <- sqldf('SELECT Article, sum(Sales_Vol) as Sales FROM coffee_join GROUP BY Article')
coffee.articletop10

coffee.articletop10.names <-
  coffee.articletop10 %>% 
  arrange(desc(Sales)) %>% 
  top_n(10) %>% 
  select(Article)

coffee.articletop10.names

coffee.top10.article.sales <- 
  coffee.article %>% 
  filter(coffee.article$Article %in% coffee.articletop10.names$Article)

coffee.top10.article.sales

library(stringr)

ggplot(coffee.top10.article.sales, aes(x=str_wrap(Article, width = 15), y=Sales)) + geom_boxplot() + theme_economist() +
  ggtitle("Top 10 Coffee Articles") + labs(x="Article", y="Sales $") + 
  theme(text = element_text(size=8), axis.text.x = element_text(angle=90, hjust=1)) + geom_smooth(method="lm")



#8
# PROMO VS REGULAR BY WEEK

coffee.promo.sales <- sqldf('SELECT Week, sum(TPR_Sales_Vol) as Promo_Sales, sum(Sales_Vol_Regular_Price) as Regular_Sales FROM
coffee_join GROUP BY Week')

coffee.promo.sales <- gather(coffee.promo.sales, type, Sales, Promo_Sales:Regular_Sales)
coffee.promo.sales$type <- as.factor(coffee.promo.sales$type)

ggplot(coffee.promo.sales, aes(x=Week, y=Sales, fill = type)) + geom_col() + theme_economist() + 
  ggtitle("Promo vs. Regular Sales by Week") + labs(x="Week", y="Sales $")



#9
# TIME SERIES ANALYSIS

library(forecast)
coffee.ts.extract <- sqldf('SELECT Week, sum(Sales_Vol) as Sales FROM coffee_join GROUP BY Week')


coffee.ts <- ts(coffee.ts.extract$Sales, start = c(2017,3), frequency = 52)
plot(coffee.ts, main ="Time Series for Coffee Sales", xlab="Weeks", ylab="Sales $")

plot(decompose(coffee.ts))

boxplot(coffee.ts~cycle(coffee.ts), main="Boxplot Showing Seasonality in Coffee Sales", xlab="Weeks", ylab="Sales $")


#10
# Missing Values
library(mice)

md.pattern(coffee_join)


coffee.missing <- sqldf('SELECT TPR_Sales_Vol as PromoSales, TPR_Unit_Vol as PromoUnits, Sales_SPPD as SalesSPPD
FROM coffee_join')
md.pattern(coffee.missing)

imputed_data <- mice(coffee.imput, m=5, maxit=30, meth='pmm', seed=1)
summary(imputed_data)



####################

cor(reg_data$Sales_Vol, reg_data$TotalPrecip)
cor(reg_data$Sales_Vol, reg_data$AverageTemp)

reg_data_1 <- sqldf('SELECT Week, Sales_Vol, AverageTemp, TotalRain, TotalSnow, TotalPrecip, RainFlag, SnowFlag, Precipflag FROM coffee_join')

reg_data <- sqldf('SELECT Week, Sales_Vol, AverageTemp, TotalRain, TotalSnow, TotalPrecip, RainFlag, SnowFlag, Precipflag FROM coffee_join')
reg_data_week <- sqldf('SELECT Week, SUM(Sales_Vol) as Sales, AverageTemp, TotalRain, TotalSnow, TotalPrecip, RainFlag, SnowFlag, Precipflag FROM coffee_join GROUP BY Week')

par(mfrow=c(1,4))

reg1 <- lm(Sales_Vol ~ AverageTemp + TotalRain + TotalSnow + TotalPrecip, reg_data)
summary(reg1)
plot(reg1)
plot(density(resid(reg1)))

reg2 <- lm(Sales_Vol ~ AverageTemp, reg_data)
summary(reg2)
plot(reg2)
plot(density(resid(reg2)))


reg3 <- lm(Sales ~ AverageTemp, reg_data_week)
summary(reg3)
plot(reg3)
plot(density(resid(reg3)))



reg4 <- lm(Sales_Vol ~ ., coffeecor)
summary(reg4)















plot(reg1$fitted.values, reg1$residuals)
abline(reg1)


reg1 <- lm(Sales_Vol~ Unit_Vol + Avg_AC_Dist+ AverageTemp+TotalPrecip, coffee_joindata)
summary(reg1)
plot(reg1)


plot(reg1$fitted.values, reg1$residuals)
abline(reg1)








