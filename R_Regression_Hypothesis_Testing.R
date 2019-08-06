#####Hypothesis Testing#####

library(readxl)
library(car)
library(estimatr)
library(tidyverse)
library(caret)


#Hypothesis_Testing.xslx
file <- read_excel(file.choose())

head(file)

reg <- lm(Y ~ X1 + X2 + X3 + X4 + X5, file)

summary(reg)


Hypo_test <- linearHypothesis(reg, c("X1 = 0", "X2 = 0", "X3=0"))




#####Chow Test#####

#Chow_Test.xslx
file <- read_excel(file.choose())

head(file)
summary(file)

reg <- lm(Y ~ X1 + X2 + X3, file)

summary(reg)

plot(reg)

plot(density(resid(reg)))

#first we need to create the extra variables!

names(file)[1] <- "Week"

file$c0 <- ifelse(file$Week < 51, 0, 1)
file$c1 <- ifelse(file$Week < 51, 0, file$X1)
file$c2 <- ifelse(file$Week < 51, 0, file$X2)
file$c3 <- ifelse(file$Week < 51, 0, file$X3)

#now, let's run a regression with the new variables

reg2 <- lm(Y ~ X1 + X2 + X3 + c0 + c1 + c2 + c3, file)

summary(reg2)

#and run the Chow test - which is really just a specific joint hypothesis test!

linearHypothesis(reg2, c("c0 = 0", "c1 = 0", "c2=0", "c3=0"))



#####Heteroskedasticity#####

#Heteroskedasticity.xslx
file <- read_excel(choose.files())

head(file)

reg <- lm(Y ~ X1 + X2, file)

summary(reg)
plot(reg)

#for some extra plots, we need to add the predictions and residuals to the original data set

file$predicted <- predict(reg)
file$residuals <- residuals(reg)

#we can dig deeper to look eat each individual variable plotted by regressor

file %>% 
  gather(key = "iv", value = "x", -Y, -predicted, -residuals, -Obs) %>%  
  ggplot(aes(x = x, y = residuals)) +  
  geom_point(aes(y = residuals), shape = 1) +
  facet_grid(~ iv, scales = "free_x") + 
  theme_bw()

ncvTest(reg) #Breusch-Pagan test

#here we use the HCCME method to produce heteroskedasticity consistent inferences and tests

rob_reg <- lm_robust(Y ~ X1 + X2, file, se_type="HC3")

summary(rob_reg)
#note how the p-values are different


