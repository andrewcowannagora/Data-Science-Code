library(tidyverse)
library(readxl)
library(sqldf)
library(stringr)
library(ggplot2)



wealth <- read_excel("C:/Users/owner/Desktop/Assignment1_Data.xlsx", sheet = "Wealth")
demo <- read_excel("C:/Users/owner/Desktop/Assignment1_Data.xlsx", sheet = "Demo")

#Question 
#1.	You have received two data sets - one Wealth, and one Demo. 
#They contain wealth and demographic data for Canadian dissemination areas. 
#The data has been slightly obfuscated, but is real and in the same format it is sold in. 
#Using SQL and sqldf, accomplish the following.


# a. What is the maximum net worth in a DA? What is the DA #? 

sqldf('select prcdda, WSWORTHWPV from q1wealth where wsworthwpv = (select max(wsworthwpv) from q1wealth)')

# b.	What is the average age in DAs with an unemployment rate that is above average?

sqldf('SELECT COUNT(PRCDDA) FROM demo WHERE STYAPT/(STYAPT + STYHOUS) < 0.5')

# c.	How many DAs are more than 50% condos?

sqldf('select avg(a.wsdebtb) from q1wealth a join q1demo b on a.prcdda = b.prcdda where b.acter < 50')
     
# d.	What is the ID, population, and total net worth of the DA with the highest net worth?

sqldf('select a.prcdda, a.baspop, b.wsworthwpv 
      from q1demo 
      a join q1wealth b on a.prcdda = b.prcdda where b.wsworthwpv = (select min(wsworthwpv) from q1wealth)')


#Question 2

#2.	Your manager has provided a dataset she knows little about. 
#She has told you that it is messy and too difficult for her to work on. 
#Now that you are an expert in R, she would like you to:


sales <- read_excel("C:/Users/owner/Desktop/Assignment1_Data.xlsx", sheet = "Question 2")

# a.	Make sure all variable formats are intuitive, and that Product_IDs are all of the same length, and a string. 

sales$Price <- as.numeric(gsub("\\$","", file$Price))

sales$Product_ID <- str_pad(file$Product_ID, 3, pad = "0")

# b.	Tidy the dataset (i.e., make sure all columns are variables, all rows are observations, and there is a single data point in each cell).

tidy_file <- gather(file, Year, Sales, Sales_2016, Sales_2017)

# c.	Provide summary statistics and visualize price. 

summary(sales)

#Plot price vs 2017 Sales
ggplot(sales, aes(y= Sales_2017, x = Price)) +
  geom_point(aes(color = sales$Import))  +
  labs(title = "Price vs. 2017 Sales",
       y = "2017 Sales",
       x = "Price",
       color = "Import")


# d. Tell a story with the data

cor(sales$Price, sales$Num_Retailers)


ggplot(sales, aes(y=Num_Retailers, x=Price)) +
  geom_point(aes(color = sales$Num_Retailers))


ggplot(sales, aes(y=Num_Retailers, x=Price))  +
  geom_point() +
  geom_smooth(method="lm", se=F) +
  labs(title = "Price | Num_Retailers",
       y = "Number of Retailers",
       x = "Price",
       color = "Num_Retailers")

## There is a negative correlation between price and number of retailers meaning that as the product price goes up there are fewer retailers.

# e.

##Select top 10 2017 Sales
sales2 <- sales[with(sales,order(-Sales_2017)),]
sales2 <- sales2[1:10,]

##Select bottom 10 2017 Sales
sales3 <- sales[with(sales,order(Sales_2017)),]
sales3 <- sales3[1:10,]

##combine
sales4 <- rbind(sales2, sales3)


ggplot(sales4, aes(y= Sales_2017, x= reorder(Product_ID, Sales_2017), fill= Sales_2017 ))  +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low="darkred", high="darkgreen")+ 
  coord_flip() +
  labs(title = "Top 10, Bottom 10 Products by 2017 Sales",
       y = "2017 Sales",
       x = "Product ID",
       color = "Product_ID")














