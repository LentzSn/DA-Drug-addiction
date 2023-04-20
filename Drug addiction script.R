#This script will contain analysis on this dataset

#Import the data
dataset <-read.table("~/MSO-6002/Umaru.csv", header=T, sep=",")
head(dataset)
summary(dataset)
#Checking if some patients have have followed more than 30 treatments
