#1) Your client has asked you to perform some analysis on the data found on the tab Missing.

#A) Estimate the model y = B0 + B1 X1 + B2 X2 + . B5 X5 using multiple imputation to correct for missing values.  

library(readxl)
library(mice)

missing <- read_excel("C:/Users/owner/Desktop/Assignment3_Data.xlsx", sheet = "Missing")
                      
reg1 <- lm(Y ~ X1 + X2 + X3 + X4 + X5, missing)
summary(reg1)

md.pattern(missing)

imputation <- mice(data = missing, m = 5, maxit = 30, method = "pmm", seed = 1)

complete(imputation, 1)

reg2 <- with(imputation, lm(Y ~ X1 + X2 + X3 + X4 + X5, missing))


#B)	According to your results, does X2 belong in that model?  Explain why / why not.  )
summary(pool(reg2)) #X2            13.828060   7.820282 1.7682304 79.06351 0.080879895


#F)
summary(reg2) #X1              6.50      3.82     1.70  0.0924... 
              #X1              6.50      3.82     1.70  0.0924...
  

#2)Wine Model to Predict Rating

wine <- read_excel("C:/Users/owner/Desktop/Assignment3_Data.xlsx", sheet = "Wine_Data")
                   
reg3 <- lm(Rating ~ ., wine)
summary(reg3)


reg4 <- lm(Rating ~ Price + Alcohol + Sulphates + Country, wine)
summary(reg4)

reg7 <- lm(Rating ~ Price + Alcohol + Sulphates, wine)
summary(reg7)


wine$CountryFrance <- as.integer(wine$Country == "France")
wine$CountryItaly <- as.integer(wine$Country == "Italy")
wine$CountryUS <- as.integer(wine$Country == "US")
wine$CountryCanada <- as.integer(wine$Country == "Canada")

reg5 <- lm(Rating ~ Price + Alcohol + Sulphates + CountryFrance + CountryItaly + CountryUS + CountryCanada, wine)
summary(reg5)

reg6 <- lm(Rating ~ Price + Alcohol + Sulphates + CountryFrance + CountryItaly, wine)
summary(reg6)
plot(reg6)


cor(wine$Rating, wine$Price)




