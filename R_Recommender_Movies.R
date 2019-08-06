#download dataset from here
#LINK: http://grouplens.org/datasets/movielens/1m/

#set wd
setwd("/Volumes/E/CSDA1050_PT_CourseMaterials_Sep2018/Week4_Class2/movie_recsys")

#ready in datasets
movies=read.csv("movies.csv")
ratings=read.csv("ratings.csv")
tags=read.csv("tags.csv")
  
#import/install folllowing packages
#install.packages("reshape2", dependencies=TRUE)
#install.packages("stringi", dependencies=TRUE)

#load librarires
library(reshape2)

#look at the datasets - the main one we will use is ratings
str(ratings)
head(ratings, 20)

#the other datasets
str(movies)
head(movies, 20)

#tags dataset (we won't be using this...)
str(tags)
head(tags, 20)

#Create ratings matrix with rows as users and columns as movies. We don't need timestamp</em>
rating_wide = dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)
head(ratings, 20)
str(ratings)
str(rating_wide)

#?dcast
#used to transpose a "melted/molten" (sometimes called dense matrix) dataset into a sparse dataset
#basic operation: dcast(<dataset>, <User> ~ <Item>, value.var = <Rating>, ...)
#note its 47.3 Mb instead of 20 mb originally
object.size(ratings)
object.size(rating_wide)

#as.matrix lets you turn the dataset into a matrix
ratingmatrix = as.matrix(rating_wide[,-1])
str(ratingmatrix)

#user 1, movie id 31
head(ratings)
ratingmatrix[1, 31]

#The recommendation package in R we'll use is recommenderlab.
#It provides us a User Based Collaborative Filtering (UBCF) model.
#For similarity among user ratings, we have a choice to calculate similarity according to the following methods:
  
#Jaccard similarity
#Cosine similarity
#Pearson similarity

<#install.packages("recommenderlab", dependencies=TRUE)</em>
library(recommenderlab)

#note for many problems of this nature, you will have a sparse matrix
#Convert ratings matrix to real rating matrx which makes it dense again -- same information but doesnt keep all N/As
ratingmatrix_dense = as(ratingmatrix, "realRatingMatrix")
#check out the size now, its 1.7 mb instead of 40+ mb


#build a recommender
#Create Recommender Model. The parameters are UBCF and Cosine similarity. We take 10 nearest neighbours
rec_mod = Recommender(ratingmatrix_dense, method = "UBCF", param=list(method="Cosine",nn=10))


#that's it. Now to actually score someone, lets try the first 5 entires
Top_5_pred = predict(rec_mod, ratingmatrix_dense[1], n=5)

#to see this - not that useful
Top_5_pred
str(Top_5_pred)

#need to convert to list & print them out
Top_5_List = as(Top_5_pred, "list")
Top_5_List

#corresonst to movie #s 257, 780, 457 -- can map these back to movies

#can do data manpulation below to tidy things up using DPLYR
library(dplyr)

#turn list into dataframe
Top_5_df=data.frame(Top_5_List)

#give it a column name
colnames(Top_5_df)="movieId"

#to join to movie dataset we need to cast as INT
Top_5_df$movieId=as.numeric(levels(Top_5_df$movieId))

#to get actual names
names=left_join(Top_5_df, movies, by="movieId")



