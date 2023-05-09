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
summary(drugtx30)

#Categorize the age variable
dataset$age_category <- NA
dataset$age_category[dataset$AGE <30]<-"Moins de 30 ans"
dataset$age_category[dataset$AGE>=30 & dataset$AGE<=35] <-"Jeunes adultes"
dataset$age_category[dataset$AGE >36] <- "Adultes"
dataset$age_category<-as.factor(dataset$age_category)

#create a variable that represent depression score divided by 10
dataset$new_beck <- dataset$BECK/10


#Select people who started taking drugs again before the end of program and are older than 35
dfree_35 <-subset(dataset, dataset$DFREE == 0 & dataset$AGE>35)
head(dfree_35)
str(dataset)
summary(dfree_35)

####################
#Univariate analysis
####################

# Histogram for Age
hist(dataset$AGE, main = "Age histogram", xlab = "Age in years")
#barplot for age_category variable
barplot(table(dataset$age_category),main = "Age category barplot", xlab="Groups")
# Histogram for depression score (Beck variable)
hist(dataset$BECK, main = "Depression score histogram", xlab = "Depression score")
# Histogram for drug treatment
hist(dataset$NDRUGTX, main = "Drug treatment histogram", xlab = "Drug treaments")
#Barplot for drugs pathways i
dataset$IVHX<-factor(dataset$IVHX,1:3,c("Never", "Old", "Recent"))
barplot(table(dataset$IVHX),main = "Intravenous utilisation", xlab="Usage")
#Barplot for race
dataset$RACE<-factor(dataset$RACE,0:1,c("White","Other"))
barplot(table(dataset$RACE),main = "Race of patients",xlab = "Race of patients")
#Barplot for treatment length
dataset$TREAT<-factor(dataset$TREAT,0:1,c("Short","Long"))
barplot(table(dataset$TREAT),main = "Treatment length",xlab = "Treatment length")
#Barplot for site distribution
dataset$SITE<-factor(dataset$SITE,0:1,c("A","B"))
barplot(table(dataset$SITE),main = "Site distribution",xlab = "Site distribution")
#Barplot to determine people who relapsed or not before the end of the program
dataset$DFREE<-factor(dataset$DFREE,0:1,c("Yes","No"))
barplot(table(dataset$DFREE),main = "Relapse before the end",xlab = "Relapse for the end of the program")

######################
#Multivariate analysis
######################

#Checking the characteristics of the subjects in terms of the treatments
boxplot(dataset$AGE~dataset$TREAT,main="Age in terms of treatment", xlab="Treatment",ylab="Age")
boxplot(dataset$BECK~dataset$TREAT,main="Depression score in terms of treatment", xlab="Treatment",ylab="Depression score")
boxplot(dataset$NDRUGTX~dataset$TREAT,main="Number of previous treaments in terms of treatment duration", xlab="Treatment",ylab="Previous treatments")
table(dataset$IVHX,dataset$TREAT)
table(dataset$RACE,dataset$TREAT)
table(dataset$SITE,dataset$TREAT)
table(dataset$DFREE,dataset$TREAT)

#Checking the characteristics of the subjects in terms of the site
boxplot(dataset$AGE~dataset$SITE,main="Age in terms of the treatment site", xlab="Site",ylab="Age")
boxplot(dataset$BECK~dataset$SITE,main="Depression score in terms of the treatment site", xlab="Site",ylab="Depression score")
boxplot(dataset$NDRUGTX~dataset$SITE,main="Number of previous treaments in terms of treatment duration", xlab="Site",ylab="Previous treatments")
table(dataset$IVHX,dataset$SITE)
table(dataset$RACE,dataset$SITE)
table(dataset$TREAT,dataset$SITE)
table(dataset$DFREE,dataset$SITE)

#Score of depression according to the site
#Site is 2 categories so t-test to determine whether whether the score is different according to the site
t.test(dataset$BECK~dataset$SITE)

#Score of depression according to the race
#Race is 2 categories so t-test to determine whether whether the score is different according to the race
t.test(dataset$BECK~dataset$RACE)

#Trying to determine whether the mean age is different according to drug utilization
anova_test <- aov(dataset$AGE~dataset$IVHX)
summary(anova_test)

#Correlation between depression score and age
cor(dataset$BECK,dataset$AGE)

#Correlation between depression score and drug utilization
cor(dataset$BECK,dataset$NDRUGTX)

#Plotting the relation between depression and age
plot(dataset$BECK~dataset$AGE, main="Plot the relation between those variables", xlab="Age",ylab="Depression score")

#Linear regression model for depression and treatment
linear_treat <-lm(dataset$BECK~dataset$TREAT)
summary(linear_treat)
#Linear regression model for depression and site
linear_site <-lm(dataset$BECK~dataset$SITE)
summary(linear_site)
#Linear regression model for depression and age
linear_age <-lm(dataset$BECK~dataset$AGE)
summary(linear_age)
#Linear regression model for depression and drug antecedent
linear_antecedent <-lm(dataset$BECK~dataset$NDRUGTX)
summary(linear_antecedent)
#Linear regression model for depression and drug relapse
linear_relapse <-lm(dataset$BECK~dataset$DFREE)
summary(linear_relapse)

#Multiple linear regression model
linear_multiple<-lm(dataset$BECK~dataset$TREAT+dataset$SITE+dataset$AGE)
summary(linear_multiple)
