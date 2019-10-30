library(MASS)
library(neuralnet)
library(dplyr)
library(randomForest)
data <- read.csv("\\A1Data_MMAI.csv", header = TRUE, sep = ",", strip.white=TRUE)
head(data)

set.seed(999)
x <- select(data,-OutputReturn)
y <- select(data,OutputReturn)
max <- apply(x, 2, max)
min <- apply(x, 2, min)
scaled <- as.data.frame(scale(x, center = min, scale = max - min))
full <- cbind(scaled, y)
full$c1 <- full$ExcessReturnfromRiskModel * full$Returns
full$c2 <- full$ExcessReturnfromRiskModel * full$b_mkt
full$c3 <- full$ExcessReturnfromRiskModel * full$b_umd
full$c4 <- full$ExcessReturnfromRiskModel * full$b_hml
full$c5 <- full$ExcessReturnfromRiskModel * full$b_smb
full$c6 <- full$Returns * full$b_mkt
full$c7 <- full$Returns * full$b_umd
full$c8 <- full$Returns * full$b_hml
full$c9 <- full$Returns * full$b_smb
full$c10 <- full$b_mkt * full$b_umd
full$c11 <- full$b_mkt * full$b_hml
full$c12 <- full$b_mkt * full$b_smb
index <- sample(seq_len(nrow(full)),size = 0.7*nrow(full))

train <- full[index,]
test <- full[-index,]

rf <- randomForest(OutputReturn~EPS+TobinsQ+b_umd+ExcessReturnfromRiskModel+b_hml+alpha+Returns+c1+c2+c3+c4+c6+c7+c8+
                     c10+c11,data=train)
varImpPlot(rf)
new <- select(full,c(EPS,TobinsQ,b_umd,ExcessReturnfromRiskModel,b_hml,alpha,Returns,c1,c2,c3,c4,c6,c7,c8,
                        c10,c11,OutputReturn))
write.csv(new,"B:\\modeling\\Data Science Files\\Chelsea Yao\\new_nn.csv")
