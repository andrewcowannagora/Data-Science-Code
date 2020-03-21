#your first regression

library(tidyverse)

library(readxl)


sales <- read_excel("C:\\Users\\Data_File_v1.0.xlsx",
           sheet = 2)

#OR sales <- file.choose()

head(sales)
str(sales)

?lm

reg <- lm(Order_Size ~ Ad_Budget + Distance, sales)

reg

summary(reg)

plot(reg)

plot(density(resid(reg)))
