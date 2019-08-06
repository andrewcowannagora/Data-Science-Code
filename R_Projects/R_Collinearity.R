# Assignment 2
library(readxl)


#2) Collinearity: collections of variables that tend to move together, such as height and weight, are called collinear.  
#This creates some challenges for analysis in that individual t-statistics tend to be less informative.  
#Using the data found on the tab 'Collinear':

#A)	Filter the data to consider only the first 25 observations then run the following models; 
#$repeat the analysis with all the observations and note any differences.   
#You do not need to worry about standard data problems such as heteroscedasticity, etc. 
#You will finish with 6 different regressions.


col <- read_excel("C:/Users/owner/Desktop/Assignment2_Data.xlsx", sheet = "Collinear")
col2 <- col[1:25,]


#i) Run a linear regression to explain y in terms of experience and height.  Does height appear to explain y?

reg1 <- lm(Y ~ Experience + Height, col)
reg2 <- lm(Y ~ Experience + Height, col2)

summary(reg1) # Height        1.2762     0.1840   6.935 4.53e-10 ***
summary(reg2) # Height        1.6693     0.5328   3.133  0.00484 ** 

# Height does have predictive significance but it is stronger in the full 100 set of observations.

#ii) Run a linear regression to explain y in terms of experience and weight.  Does weight appear to explain y?

reg3 <- lm(Y ~ Experience + Weight, col)
reg4 <- lm(Y ~ Experience + Weight, col2)

summary(reg3) # Weight        1.5527     0.2838   5.472 3.48e-07 *** 
summary(reg4) # Weight        1.5973     0.7348   2.174   0.0408 *

# Weight does have significant predictive power in the full set of observations. In the 25 observation set it has very little predictive ability 
# since the p-value is very high at 0.04.


#iii) Run a linear regression to explain y in terms of experience and height and weight. 

reg5 <- lm(Y ~ Experience + Height + Weight, col)
reg6 <- lm(Y ~ Experience + Height + Weight, col2)

summary(reg5) #Height       1.29684    0.34988   3.707 0.000351 ***
              #Weight      -0.03507    0.50464  -0.069 0.944743     

summary(reg6) #Height        2.0557     0.9967   2.063   0.0517 .  
              #Weight       -0.5822     1.2598  -0.462   0.6487    


#B) Do these variables appear to be significant when considered individually?

#When included in the model together, height and weight become far less statistically significant individually.



