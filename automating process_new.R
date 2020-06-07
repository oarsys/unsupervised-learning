## install packages dplyr and dbscan only for the first time
install.packages("dbscan")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("magrittr")
install.packages("xlsx")
## call the packages every time
library(dplyr)
library(dbscan)
library(xlsx)
library(readxl)
## put your file path in the parenthesis
setwd("c:/Users/xushu/Documents/UMN/consulting/")

## put the filename in the parenthesis
rawdat=read_excel("2016 stats.xlsx")

#In this context price refers to product.price.
#transform to numeric values
rawdat$num.price=rawdat$`product price`
#add one column to store the original indices: 
rawdat$index=1:dim(rawdat)[1]
#indices: price=0


## wrong results
na.index=which(is.na(rawdat$num.price))
zero.index=which(rawdat$num.price<0.5)
dat=subset(rawdat,num.price>=0.5)



library(magrittr)
count<- (dat %>% group_by(dat$PRODUCTID) %>% summarise(count=n()))
id<-count[count$count<5, ]
Xdatnon<-merge(id, dat, by.x=c("dat$PRODUCTID"), by.y=c("PRODUCTID"),all=T)
summary(Xdatnon$count)
Xdatnon<-filter(Xdatnon[!is.na(Xdatnon$count), ])
id2<-count[count$count>=5, ]
Xdat<-merge(id2, dat, by.x=c("dat$PRODUCTID"), by.y=c("PRODUCTID"),all=T)
summary(Xdat$count)
Xdat<-filter(Xdat[!is.na(Xdat$count), ])
dim(Xdat)


library(tidyverse)
list.data=lapply(split(Xdat$num.price, Xdat$PRODUCT_, drop = TRUE),as.matrix)
index.data=lapply(split(Xdat$index, Xdat$PRODUCT_, drop = TRUE),as.matrix)

## eps = 10 ($10 criteria) and at least 3 price entries to form a cluster
res.data=lapply(list.data,dbscan,eps=10,minPts=3)
cluster.data=lapply(res.data, '[[', "cluster")
m=mapply(cbind,X=index.data,Y=lapply(cluster.data,as.matrix))
most.fre=function(x){tt=sort(table(x[,2]),decreasing=T)
#there exists multiple equally large groups
l=length(which(tt==tt[1]))
temp=as.numeric(names(tt))
temp=temp[temp!=0]
return(x[which(x[,2] %in% temp[1:l]),])}



good.data=lapply(m,most.fre)
#throw away those NA's caused by noise group being the largest group
good.data=good.data[lapply(good.data,function(x)dim(x)[1]>0)==T]
#multiple largest clusters
mul.good.data=good.data[lapply(good.data,function(x)min(x[,2])==max(x[,2]))==F]
#only one largest cluster
one.good.data=good.data[lapply(good.data,function(x)min(x[,2])==max(x[,2]))==T]

length(unlist(one.good.data))/2

## potentially good
mul.good = lapply(mul.good.data, function(x){x=x[,1];x})
mul.index = unlist(mul.good,use.names = F)
rawdat$mul.index<-sort(mul.index)

## most likely good
one.good = lapply(one.good.data,function(x){x=x[,1];x})
one.index = unlist(one.good,use.names = F)

## wrong entries
wrong.index<-sort(c(na.index,zero.index))
## potentially wrong entries
potentially.wrong.index<-sort(rawdat$index[-c(mul.index, one.index, wrong.index)])

rawdat$property<-rep(0,dim(rawdat)[1])
rawdat$property[one.index]="most.likely.good"
rawdat$property[mul.index]="potentially.good"
rawdat$property[potentially.wrong.index]="potentially.wrong"
rawdat$property[wrong.index]="wrong"

## put your desired file path in the parenthesis
setwd("c:/Users/xushu/Documents/UMN/")

## put the desired filepath+filename.csv in the parenthesis
write.csv(rawdat, file = "c:/Users/xushu/Documents/UMN/output.csv")
