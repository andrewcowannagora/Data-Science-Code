
install.packages("mice")

library(mice)
library(readxl)


#file is Data_File_v2.0
file<-read_excel(file.choose())

head(file)

reg1 <- lm(Grocery_Bill ~ N_Adults + Family_Income + N_Vehicles + Distance_to_Store + Vegetarian + N_Children + Family_Pet, file)

summary(reg1) #note how it deletes the observations with missing records.

md.pattern(file) #this tells us that there are 977 observations with no missing values, and 23 obs missing family_income

imputed_data <- mice(file, m=5, maxit=30, meth='pmm', seed=1)
#for more advanced imputation, try typing methods(mice) in the console

summary(imputed_data)

#the complete function will let you pick any single set of imputed data

completed_data <- complete(imputed_data, 1)

head(completed_data)
summary(completed_data)
summary(file) #note the missing NAs in Family_Income

#however, for linear modelling, this is not the best choice - instead, we should pool the results

reg1 <- with(imputed_data, lm(Grocery_Bill ~ N_Adults + Family_Income + N_Vehicles + Distance_to_Store + Vegetarian + N_Children + Family_Pet))
summary(reg1)
summary(pool(reg1))

#these commands complete a regression on each of the 5 imputed data sets, and then pools the results
#you can now interpret as if it were a regular regression. 

