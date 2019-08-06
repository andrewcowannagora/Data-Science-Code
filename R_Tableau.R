
######## Setting up & running the Rserver

install.packages("pacman")

pacman::p_load("Rserve")

Rserve()

####### Set fake vars

.arg1<-seq(1:35)

########### Comparative data

l<-length(.arg1)
norm<-rnorm(l,0,l)
norm

################  scaling

  scale(.arg1)

########### simple data manipulations

l<-length(.arg1)
s<-seq(1,l)

mid.s<-round(l/2,0)

a=-2
b=-mid.s*2*a
y=a*s^2+b*s

plot(y)

######### Reading RSS into Tableau

pacman::p_load(feedeR)

l<-length(.arg1)
stock.feed <- feed.extract("http://articlefeeds.nasdaq.com/nasdaq/categories?category=Stocks") 
articles<-stock.feed$items$title
articles[1:l]

########### Scraping a website

 pacman::p_load(rvest,stringr)
 l<-length(.arg1)
 CBCFrontPage <- read_html("http://www.cbc.ca/news")
 
 nodes<-html_nodes(CBCFrontPage, "p , .pinnableHeadline")
 strings<-html_text(nodes)
 strings<-gsub("\\s+"," ",strings, fixed=FALSE)
 strings<-gsub("\\n+"," ",strings,fixed=FALSE)
 strings<-gsub("\\t+"," ",strings,fixed=FALSE)
 strings<-strings[nchar(strings)>5]
 
 strings[1:l]
  
##############################


