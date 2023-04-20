#This script will contain analysis on the drug addiction dataset

#Import the data
dataset <-read.table("~/MSO-6002/Umaru.csv", header=T, sep=",")
head(dataset)
summary(dataset)

#Checking if some patients have have followed more than 30 treatments
summary(dataset$NDRUGTX)
drugtx30 <- subset(dataset, dataset$NDRUGTX>30)
head(drugtx30)
tail(drugtx30)

#Categorize the age variable
dataset$age_category <- NA
dataset$age_category[dataset$AGE <30]<-"Moins de 30 ans"
dataset$age_category[dataset$AGE>=30 & dataset$AGE<=35] <-"Jeunes adultes"
dataset$age_category[dataset$AGE >=36] <- "Adultes"
dataset$age_category<-as.factor(dataset$age_category)

#create a variable that represent depression score divided by 10
dataset$new_beck <- dataset$BECK/10

#Select people who went stated taking drugs again before the end of program and are older than 35
dfree_35 <-subset(dataset, dataset$DFREE == 0 & dataset$AGE>35)
head(dfree_35)