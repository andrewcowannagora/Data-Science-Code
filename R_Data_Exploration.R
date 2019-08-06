my_file <- read.csv("C:\\Users\\Grocery_Data.csv", 
                    header=TRUE, 
                    sep = ",")

#OR my_file <- file.choose()

library(tidyverse)

head(my_file)
str(my_file)
summary(my_file)

my_file$Grocery.Bill <- as.numeric(gsub("\\$","", my_file$Grocery_Bill))

my_file$Family_Income <- as.numeric(gsub( "[$,]", "", my_file$Family_Income))

str(my_file)
summary(my_file)

#DPLYR

my_filtered_file <- filter(my_file, N_Children == 0 & N_Vehicles == 0)

head(my_filtered_file)

my_arranged_file <- arrange(my_file, desc(Grocery_Bill))

head(my_arranged_file)

my_selected_file <- select(my_file, Grocery_Bill, Family_Income, Family_Size)

head(my_selected_file)

#GGPLOT2

my_graph <- ggplot(my_file, aes(y=Family_Income, x=Grocery_Bill)) + geom_point()
my_graph


my_graph2 <- ggplot(my_file, aes(y=Family_Income, x=Grocery_Bill))  + #plot family income vs. grocery bill
  geom_point(aes(shape = 15, color=my_file$Family_Size))  + #make it a scatter plot, colored by family size
  geom_smooth(method="lm", se=F) + #use linear regression draw a line of best fit
  scale_shape_identity() + #used to allow continous variables to change point shape
  xlim(c(0,600)) + #change the x axis
  ylim(c(0,150000)) + #change the y axis
  labs(title = "Grocery Bill vs. Family Income", #add labels
         y = "Family Income",
         x = "Grocery Bill",
        color = "Family Size")

my_graph2

#TIDYR
#we will have to create some data for this - it is easy to create some fake stock data 
set.seed(10)
stocks <- data.frame(
  time = as.Date('2018-01-01') + 0:9,
  S1 = rnorm(10, 50, 10), #10 data points, mean of 5, std dev of 10
  S2 = rnorm(10, 10, .5),
  S3 = rnorm(10, 1, 1)
)

head(stocks) 

gathered_stock <- gather(stocks, stock, price, S1,S2,S3)

head(gathered_stock, n= 20)

spread_stock <- spread(gathered_stock, stock, price)


head(spread_stock)

#stringr
weird_string <- c("    Hello", "    World!", " a", "   b")

weird_string_fixed <- str_trim(weird_string)

weird_string_fixed

weird_string_padded <- str_pad(weird_string_fixed, 12, pad = "0")

weird_string_padded
