# E-Commerce Clustering


library(tidyverse)
library(dplyr)
library(arules)
library(readxl)
library(cluster)
install.packages("factoextra")
install.packages("dendextend")
library(factoextra) # clustering visualization
library(dendextend)


#Your Client
#Your client is an online grocery retailer based out of the UK that sells a variety of unique gifts
#for many occasions. Much of their sales are to wholesalers but they also sell direct to consumers. 

# Product Clustering
# . Run hierarchical clustering to group products together based on how often they are purchased together (decide if it should be in the same transaction, 
#  or just by the same customer over a given time period?)
# . Cut your dendrogram wherever you think is reasonable and provide meaningful names for the product clusters that are formed at that point
# . Identify the product cluster with the lowest unit sales


#download data from: https://archive.ics.uci.edu/ml/datasets/Online+Retail

#loaded file
OR_data <- read_excel(file.choose()) 
summary(OR_data)

#Converted customerID into char
OR_data$CustomerID <- as.character(OR_data$CustomerID)

#Removing cancelled orders and returned items/negative unit prices
OR_data <- OR_data %>% filter(OR_data$Quantity > 0)
OR_data <- OR_data %>% filter(OR_data$UnitPrice > 0)

#Creating Revenue column
OR_data$TotalPrice <- OR_data$Quantity*OR_data$UnitPrice
head(OR_data)

#checking for missing values in all columns
OR_data %>% is.na %>% colSums


#customers per country
table(OR_data$Country)

#selecting invoiceNo and stockcode
OR_data_cluster <- OR_data[,c(1,2)]
OR_data_cluster %>% is.na %>% colSums

OR_data_cluster$InvoiceNo <- as.factor(OR_data_cluster$InvoiceNo)
OR_data_cluster$StockCode <- as.character(OR_data_cluster$StockCode)

#converting into transactions object, splitting two cols invoice numbers and stock codes
OR_data_clustHC <- as(split(OR_data_cluster$InvoiceNo, OR_data_cluster[,"StockCode"]), "transactions")
summary(OR_data_clustHC)

#clustering
#calculating distance
pclust_c <- dissimilarity(OR_data_clustHC, method = "cosine")
pclust_j <- dissimilarity(OR_data_clustHC, method = "jaccard")
pclust_e <- dissimilarity(OR_data_clustHC, method = "euclidean")
#pclust_a <- dissimilarity(OR_data_clustHC, method = "affinity")

#calculating agglomerative coefficient for ward method with cosine, jaccard and euclidean distance
agnes(pclust_c, diss = TRUE,
      stand = FALSE, method = "ward")

agnes(pclust_j, diss = TRUE,
      stand = FALSE, method = "ward")

agnes(pclust_e, diss = TRUE,
      stand = FALSE, method = "ward2")

#calculating agglomerative coefficient for single method with cosine, jaccard and euclidean distance
agnes(pclust_c, diss = TRUE,
      stand = FALSE, method = "single")

agnes(pclust_j, diss = TRUE,
      stand = FALSE, method = "single")

agnes(pclust_e, diss = TRUE,
      stand = FALSE, method = "single")

#calculating agglomerative coefficient for average method with cosine, jaccard and euclidean distance
agnes(pclust_c, diss = TRUE,
      stand = FALSE, method = "average")

agnes(pclust_j, diss = TRUE,
      stand = FALSE, method = "average")

agnes(pclust_e, diss = TRUE,
      stand = FALSE, method = "average")

#calculating agglomerative coefficient for complete method with cosine, jaccard and euclidean distance
agnes(pclust_c, diss = TRUE,
      stand = FALSE, method = "complete")

agnes(pclust_j, diss = TRUE,
      stand = FALSE, method = "complete")

agnes(pclust_e, diss = TRUE,
      stand = FALSE, method = "complete")

#HClustering, converting distance matrix to hierarchical clustering, can use single, complete, average

hc_ward_EBest = hclust(pclust_e, method = "ward.D2")

#hc_ward_c = hclust(pclust_c, method = "ward.D2")
#hc_ward_j = hclust(pclust_j, method = "ward.D2")
#hc_ward_e = hclust(pclust_e, method = "ward.D2")

#hc_single_c = hclust(pclust_c, method = "single")
#hc_single_j = hclust(pclust_j, method = "single")
#hc_single_e = hclust(pclust_e, method = "single")

#hc_mean_c = hclust(pclust_c, method = "complete")
#hc_mean_j = hclust(pclust_j, method = "complete")
#hc_mean_e = hclust(pclust_e, method = "complete")

#hc_average_c = hclust(pclust_c, method = "average")
#hc_average_j = hclust(pclust_j, method = "average")
#hc_average_e = hclust(pclust_e, method = "average")

#plot Hcluster tree
plot(hc_ward_EBest ,main="Dendrogram", xlab="Products", cex=.9)
rect.hclust(hc_ward_EBest, k = 6 , border = 2:5)

#plot(hc_ward_c ,main="Dendrogram", xlab="Products", cex=.9)
#plot(hc_ward_j ,main="Dendrogram", xlab="Products", cex=.9)
#plot(hc_ward_e ,main="Dendrogram", xlab="Products", cex=.9)

#plot(hc_single_c ,main="Dendrogram", xlab="Products", cex=.9)
#plot(hc_single_j ,main="Dendrogram", xlab="Products", cex=.9)
#plot(hc_single_e ,main="Dendrogram", xlab="Products", cex=.9)

#plot(hc_mean_c ,main="Dendrogram", xlab="Products", cex=.9)
#plot(hc_mean_j ,main="Dendrogram", xlab="Products", cex=.9)
#plot(hc_mean_e ,main="Dendrogram", xlab="Products", cex=.9)

#plot(hc_average_c ,main="Dendrogram", xlab="Products", cex=.9)
#plot(hc_average_j ,main="Dendrogram", xlab="Products", cex=.9)
#plot(hc_average_e ,main="Dendrogram", xlab="Products", cex=.9)

# showing possible clusters on dendrogram, k is the no of clusters

product_clust_w_e6<-cutree(hc_ward_EBest, k = 6)
product_clust_w_e8<-cutree(hc_ward_EBest, k = 8)
product_clust_w_e10<-cutree(hc_ward_EBest, k = 10)
product_clust_w_e12<-cutree(hc_ward_EBest, k = 12)

fviz_nbclust(OR_data_cluster, FUN = hcut, method = "silhouette")

#product_clust_w_c6<-cutree(hc_ward_c, k = 6)
#product_clust_w_c8<-cutree(hc_ward_c, k = 8)
#product_clust_w_c10<-cutree(hc_ward_c, k = 10)
#product_clust_w_c12<-cutree(hc_ward_c, k = 12)

#product_clust_w_j6<-cutree(hc_ward_j, k = 6)
#product_clust_w_j8<-cutree(hc_ward_j, k = 8)
#product_clust_w_j10<-cutree(hc_ward_j, k = 10)
#product_clust_w_j12<-cutree(hc_ward_j, k = 12)

#product_clust_w_e6<-cutree(hc_ward_e, k = 6)
#product_clust_w_e8<-cutree(hc_ward_e, k = 8)
#product_clust_w_e10<-cutree(hc_ward_e, k = 10)
#product_clust_w_e12<-cutree(hc_ward_e, k = 12)

#product_clust_s_c6<-cutree(hc_single_c, k = 6)
#product_clust_s_c8<-cutree(hc_single_c, k = 8)
#product_clust_s_c10<-cutree(hc_single_c, k = 10)
#product_clust_s_c12<-cutree(hc_single_c, k = 12)

#product_clust_s_j6<-cutree(hc_single_j, k = 6)
#product_clust_s_j8<-cutree(hc_single_j, k = 8)
#product_clust_s_j10<-cutree(hc_single_j, k = 10)
#product_clust_s_j12<-cutree(hc_single_j, k = 12)

#product_clust_s_e6<-cutree(hc_single_e, k = 6)
#product_clust_s_e8<-cutree(hc_single_e, k = 8)
#product_clust_s_e10<-cutree(hc_single_e, k = 10)
#product_clust_s_e12<-cutree(hc_single_e, k = 12)

#product_clust_a_c6<-cutree(hc_average_c, k = 6)
#product_clust_a_c8<-cutree(hc_average_c, k = 8)
#product_clust_a_c10<-cutree(hc_average_c, k = 10)
#product_clust_a_c12<-cutree(hc_average_c, k = 12)

#product_clust_a_j6<-cutree(hc_average_j, k = 6)
#product_clust_a_j8<-cutree(hc_average_j, k = 8)
#product_clust_a_j10<-cutree(hc_average_j, k = 10)
#product_clust_a_j12<-cutree(hc_average_j, k = 12)

#product_clust_a_e6<-cutree(hc_average_e, k = 6)
#product_clust_a_e8<-cutree(hc_average_e, k = 8)
#product_clust_a_e10<-cutree(hc_average_e, k = 10)
#product_clust_a_e12<-cutree(hc_average_e, k = 12)

#product_clust_m_c6<-cutree(hc_mean_c, k = 6)
#product_clust_m_c8<-cutree(hc_mean_c, k = 8)
#product_clust_m_c10<-cutree(hc_mean_c, k = 10)
#product_clust_m_c12<-cutree(hc_mean_c, k = 12)

#product_clust_m_j6<-cutree(hc_mean_j, k = 6)
#product_clust_m_j8<-cutree(hc_mean_j, k = 8)
#product_clust_m_j10<-cutree(hc_mean_j, k = 10)
#product_clust_m_j12<-cutree(hc_mean_j, k = 12)

#product_clust_m_e6<-cutree(hc_mean_e, k = 6)
#product_clust_m_e8<-cutree(hc_mean_e, k = 8)
#product_clust_m_e10<-cutree(hc_mean_e, k = 10)
#product_clust_m_e12<-cutree(hc_mean_e, k = 12)

#Convert product_clust into a data frame

product_clust_w_e6<- as.data.frame(product_clust_w_e6)
product_clust_w_e8<- as.data.frame(product_clust_w_e8)
product_clust_w_e10<- as.data.frame(product_clust_w_e10)
product_clust_w_e12<- as.data.frame(product_clust_w_e12)
hc_ward_EBest_f<-as.data.frame(hc_ward_EBest_f)
head(product_clust_w_e6)

#product_clust_w_c6<- as.data.frame(product_clust_w_c6)
#product_clust_w_c8<- as.data.frame(product_clust_w_c8)
#product_clust_w_c10<- as.data.frame(product_clust_w_c10)
#product_clust_w_c12<- as.data.frame(product_clust_w_c12)
#head(product_clust_w_c6)

#product_clust_w_j6<- as.data.frame(product_clust_w_j6)
#product_clust_w_j8<- as.data.frame(product_clust_w_j8)
#product_clust_w_j10<- as.data.frame(product_clust_w_j10)
#product_clust_w_j12<- as.data.frame(product_clust_w_j12)
#head(product_clust_w_j6)

#product_clust_w_e6<- as.data.frame(product_clust_w_e6)
#product_clust_w_e8<- as.data.frame(product_clust_w_e8)
#product_clust_w_e10<- as.data.frame(product_clust_w_e10)
#product_clust_w_e12<- as.data.frame(product_clust_w_e12)
#head(product_clust_w_e6)


#product_clust_s_c6<- as.data.frame(product_clust_s_c6)
#product_clust_s_c8<- as.data.frame(product_clust_s_c8)
#product_clust_s_c10<- as.data.frame(product_clust_s_c10)
#product_clust_s_c12<- as.data.frame(product_clust_s_c12)
#head(product_clust_s_c6)

#product_clust_s_j6<- as.data.frame(product_clust_s_j6)
#product_clust_s_j8<- as.data.frame(product_clust_s_j8)
#product_clust_s_j10<- as.data.frame(product_clust_s_j10)
#product_clust_s_j12<- as.data.frame(product_clust_s_j12)
#head(product_clust_s_j6)

#product_clust_s_e6<- as.data.frame(product_clust_s_e6)
#product_clust_s_e8<- as.data.frame(product_clust_s_e8)
#product_clust_s_e10<- as.data.frame(product_clust_s_e10)
#product_clust_s_e12<- as.data.frame(product_clust_s_e12)
#head(product_clust_s_e6)


#product_clust_a_c6<- as.data.frame(product_clust_a_c6)
#product_clust_a_c8<- as.data.frame(product_clust_a_c8)
#product_clust_a_c10<- as.data.frame(product_clust_a_c10)
#product_clust_a_c12<- as.data.frame(product_clust_a_c12)
#head(product_clust_a_c6)

#product_clust_a_j6<- as.data.frame(product_clust_a_j6)
#product_clust_a_j8<- as.data.frame(product_clust_a_j8)
#product_clust_a_j10<- as.data.frame(product_clust_a_j10)
#product_clust_a_j12<- as.data.frame(product_clust_a_j12)
#head(product_clust_a_j6)

#product_clust_a_e6<- as.data.frame(product_clust_a_e6)
#product_clust_a_e8<- as.data.frame(product_clust_a_e8)
#product_clust_a_e10<- as.data.frame(product_clust_a_e10)
#product_clust_a_e12<- as.data.frame(product_clust_a_e12)
#head(product_clust_a_e6)

#product_clust_m_c6<- as.data.frame(product_clust_m_c6)
#product_clust_m_c8<- as.data.frame(product_clust_m_c8)
#product_clust_m_c10<- as.data.frame(product_clust_m_c10)
#product_clust_m_c12<- as.data.frame(product_clust_m_c12)
#head(product_clust_m_c6)

#product_clust_m_j6<- as.data.frame(product_clust_m_j6)
#product_clust_m_j8<- as.data.frame(product_clust_m_j8)
#product_clust_m_j10<- as.data.frame(product_clust_m_j10)
#product_clust_m_j12<- as.data.frame(product_clust_m_j12)
#head(product_clust_m_j6)

#product_clust_m_e6<- as.data.frame(product_clust_m_e6)
#product_clust_m_e8<- as.data.frame(product_clust_m_e8)
#product_clust_m_e10<- as.data.frame(product_clust_m_e10)
#product_clust_m_e12<- as.data.frame(product_clust_m_e12)
#head(product_clust_m_e6)

#getwd

#Shows the clusters and based on tranactions/invoice no. it clustered the products

#Converting the datapoints to clusters

fviz_nbclust(hc_ward_EBest_f, FUN = hcut, method = "wss")

#gap_stat <- clusGap(OR_data, FUN = hcut, nstart = 25, K.max = 12, B = 50)
#fviz_gap_stat(gap_stat)


write.csv(product_clust_w_e6, "/global/home/mma_jarshad/prod_clust_w_e6.csv")
write.csv(product_clust_w_e8, "/global/home/mma_jarshad/prod_clust_w_e8.csv")
write.csv(product_clust_w_e10, "/global/home/mma_jarshad/prod_clust_w_e10.csv")
write.csv(product_clust_w_e12, "/global/home/mma_jarshad/prod_clust_w_e12.csv")

write.csv(OR_data, "/global/home/mma_jarshad/OR_data.csv")


#count the number of clusters in Excel


