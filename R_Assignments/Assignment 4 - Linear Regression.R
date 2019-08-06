##4

##A)
library(readxl)
MMA_860_2020W_Exam_Data_A <- read_excel("Assignment4_Data.xlsx", sheet = "placebook")
                                        
summary(placebook)
str(placebook)
pairs(placebook)

reg <- lm(Time_Min ~ Age + Family_Income + Number_of_Friends + Male + Student + Computer_Type, placebook)
summary(reg)
plot(reg)
plot(density(resid(reg)))

placebook$windows <- as.integer(placebook$Computer_Type == "W")
placebook$mac <- as.integer(placebook$Computer_Type == "M")
placebook$other <- as.integer(placebook$Computer_Type == "O")


reg2 <- lm(Time_Min ~ Age + Family_Income + Number_of_Friends + Male + Student + windows + mac, placebook)
summary(reg2)


##B)
reg3 <- lm(Time_Min ~ Family_Income + Number_of_Friends + Male + Student, placebook)
summary(reg3)
plot(reg3)
plot(density(resid(reg3)))

train <- placebook[1:70,]
test <- placebook[71:100,]

reg4 <- lm(Time_Min ~ Family_Income + Number_of_Friends + Male + Student, train)
summary(reg4)

pred <- predict(reg4,test)
pred

data.frame( R2 = R2(pred, test$Time_Min),
            RMSE = RMSE(pred, test$Time_Min),
            MAE = MAE(pred, test$Time_Min))

##C)

cor(placebook$Family_Income, placebook$mac)

reg5 <- lm(placebook$Family_Income ~ + mac, placebook)
summary(reg5)

ggplot(placebook, aes(x = mac, y = Family_Income)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")



##5)


##A)

names(truck)[5] <- "Make"

summary(truck)
str(truck)

reg6 <- lm(Price ~ Kms + N_Doors + Make, truck)
summary(reg6)
plot(reg6)



##C)
cor(truck$Price, truck$GM)
cor(truck$Price, truck$Ford)

truck$GM <- as.integer(truck$Make == "GM")
truck$Ford <- as.integer(truck$Make == "Ford")

reg7 <- lm(Price ~ Kms + N_Doors + GM, truck)
summary(reg7)
plot(reg7)


reg8 <- lm(Price ~ Kms + N_Doors + GM, truck)
summary(reg8)
linearHypothesis(reg8, c("GM = 0"))


reg9 <- lm(Price ~ Ford, truck)
summary(reg9)

reg10 <- lm(Price ~ GM, truck)
summary(reg10)

Hypo_test <- linearHypothesis(reg10, c("GM = 0"))
Hypo_test





