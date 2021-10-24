install.packages("randomForest")
library(randomForest)
install.packages("ElemStatLearn")
library(ElemStatLearn)

#rpubs.com/datascientiest/270685

setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Enviroment")
Accessions_data <- read.table(file="Data_traits_short.csv", header=T, sep=",")
Enviromental_data <- read.table(file="shiny climatesd short.csv", header=T, sep=",")

colnames(Enviromental_data)

#Quarter:
#WorldClim BIO16
#CHELSA BIO11
#Month:
#WorldClim BIO13
#WorldClim BIO6

#Excluding the eastern ereas:
Accessions_data<-Accessions_data[!(Accessions_data$Country=="AFG"),]
Accessions_data<-Accessions_data[!(Accessions_data$Country=="USA"),]
Accessions_data<-Accessions_data[!(Accessions_data$Country=="CAN"),]
Accessions_data<-Accessions_data[!(Accessions_data$Country=="CHN"),]
Accessions_data<-Accessions_data[!(Accessions_data$Country=="CPV"),]
Accessions_data<-Accessions_data[!(Accessions_data$Country=="UZB"),]
Accessions_data<-Accessions_data[!(Accessions_data$Country=="TJK"),]
Accessions_data<-Accessions_data[!(Accessions_data$Country=="IND"),]
Accessions_data<-Accessions_data[!(Accessions_data$Country=="JPN"),]
Accessions_data<-Accessions_data[!(Accessions_data$Country=="KAZ"),]
Accessions_data<-Accessions_data[!(Accessions_data$Country=="KGZ"),]
Accessions_data<-Accessions_data[!(Accessions_data$Country=="MAR"),]


#Subseting the parameters:
Enviromental_Quarter <-Enviromental_data[,c(1,2,91,115, 42)]
Enviromental_Month <-Enviromental_data[,c(1,2,88,81, 42)]
Accessions_1 <- Accessions_data[,c(1,20)]
Accessions_2 <- Accessions_data[,c(20, 15:17)]
Data_Quarter_1 <- merge(Accessions_1, Enviromental_Quarter, by="CS")
Data_Quarter <-Data_Quarter_1[,-c(1)]
Data_Month_1<- merge(Accessions_1, Enviromental_Month, by="CS")
Data_Month <- Data_Month_1[,-c(1)]


Data_Quarter$WC2_BIO16<- as.numeric(Data_Quarter$WC2_BIO16)
Data_Quarter$SRTM_elevation<- as.numeric(Data_Quarter$SRTM_elevation)
Data_Quarter <- na.omit(Data_Quarter) 


Data_Month$WC2_BIO13<- as.numeric(Data_Month$WC2_BIO13)
Data_Month$SRTM_elevation<- as.numeric(Data_Month$SRTM_elevation)
Data_Month <- na.omit(Data_Month) 


#Subseting the data by geography:

Accessions_data$GEO <- NA
Accessions_data$GEO[which(Accessions_data$Country == "ESP")] <- "Iberian_Peninsula"
Accessions_data$GEO[which(Accessions_data$Country == "POR")] <- "Iberian_Peninsula"
Accessions_data$GEO[is.na(Accessions_data$GEO)] <- "Europe"

Accessions_data$GEO <- as.factor(Accessions_data$GEO)



colnames(Accessions_data)

Enviromental_Quarter <-Enviromental_data[,c(1,2,91,115, 42)]
Enviromental_Month <-Enviromental_data[,c(1,2,88,81, 42)]
Accessions_1 <- Accessions_data[,c(1,20,31)]
Accessions_2 <- Accessions_data[,c(20, 15:17,31)]
Data_Quarter_1 <- merge(Accessions_1, Enviromental_Quarter, by="CS")
Data_Quarter <-Data_Quarter_1[,-c(1)]
Data_Month_1<- merge(Accessions_1, Enviromental_Month, by="CS")
Data_Month <- Data_Month_1[,-c(1)]

Data_Quarter_GEO <- split(Data_Quarter, Data_Quarter$GEO)
list2env(Data_Quarter_GEO, envir=.GlobalEnv)
rm(Data_Quarter_GEO)
Data_Quarter_GEOIberian_Peninsula <- droplevels.data.frame(Iberian_Peninsula)
Data_Quarter_GEOEurope <- droplevels.data.frame(Europe)

Data_Month_GEO <- split(Data_Month, Data_Month$GEO)
list2env(Data_Month_GEO, envir=.GlobalEnv)
rm(Data_Month_GEO)
Data_Month_GEOIberian_Peninsula <- droplevels.data.frame(Iberian_Peninsula)
Data_Month_GEOEurope <- droplevels.data.frame(Europe)


#Subseting the data by chemotypes:

#Iberian Peninsula:


#Quarter:
str(Data_Quarter_GEOIberian_Peninsula)
Data_Quarter_GEOIberian_Peninsula<-Data_Quarter_GEOIberian_Peninsula[!(Data_Quarter_GEOIberian_Peninsula$Classification_name=="3OHP"),]
Data_Quarter_GEOIberian_Peninsula<-Data_Quarter_GEOIberian_Peninsula[!(Data_Quarter_GEOIberian_Peninsula$Classification_name=="4MSO"),]
Data_Quarter_GEOIberian_Peninsula<-Data_Quarter_GEOIberian_Peninsula[!(Data_Quarter_GEOIberian_Peninsula$Classification_name=="Butenyl"),]


Data_Quarter_GEOIberian_Peninsula$WC2_BIO16<- as.numeric(Data_Quarter_GEOIberian_Peninsula$WC2_BIO16)
Data_Quarter_GEOIberian_Peninsula$SRTM_elevation<- as.numeric(Data_Quarter_GEOIberian_Peninsula$SRTM_elevation)
Data_Quarter_GEOIberian_Peninsula <- na.omit(Data_Quarter_GEOIberian_Peninsula) 
Data_Quarter_GEOIberian_Peninsula <- droplevels(Data_Quarter_GEOIberian_Peninsula) 


train <- sample(1:nrow(Data_Quarter_GEOIberian_Peninsula), nrow(Data_Quarter_GEOIberian_Peninsula)*2/3)
spam_train <- Data_Quarter_GEOIberian_Peninsula[train,]
spam_test <- Data_Quarter_GEOIberian_Peninsula[-train,]
summary(spam_train)
spam.bag <- randomForest(Classification_name~., data=spam_train, mtry=(dim(Data_Quarter_GEOIberian_Peninsula)[2]-1))
spam.bag

yhat=spam.bag$predicted
y=spam_train$Classification_name # training data
mean(y != yhat) # out of bag error rate for training data

#Apply it to the test data and get the confusion matrix and error rate:

yhat = predict(spam.bag, spam_test)
y = spam_test$Classification_name
table(y,yhat)
mean(y != yhat)

m = round(sqrt(dim(spam_train)[2]-1)) 
rm(oob_err)
for (i in c(500,1000,2000, 5000)){ # try three? different values for number of trees
  for (j in c(m-1, m, m+1)){ # tree three different values for number of predictors to sample
    set.seed(123)
    rf.spam=randomForest(Classification_name ~., data=spam_train,  mtry=j, ntree=i)
    # get oob error rate for training data
    yhat=rf.spam$predicted
    y=spam_train$Classification_name
    error_rate <- mean(y != yhat)
    if (exists('oob_err')==FALSE){
      oob_err = c(i,j,error_rate) # create initial data frame
    }
    else{
      oob_err = rbind(oob_err, c(i,j,error_rate)) # append to data frame of error rates, ntree, and mtry
    }
  }
}


oob_err <- as.data.frame(oob_err) 
names(oob_err) <- c('ntree', 'mtry', 'oob_error_rate') 
oob_err$mtry <- as.factor(oob_err$mtry) 
ggplot(oob_err, aes(x=ntree, y=oob_error_rate, group=mtry)) + geom_line(aes(color=mtry)) 

## get parameters for best tree:
ntree_x <- oob_err$ntree[which.min(oob_err$oob_error_rate)] 
ntree_x

# find mtry that minimizes oob_error_rate:
mtry_x <- as.numeric(levels(oob_err$mtry[which.min(oob_err$oob_error_rate)])[oob_err$mtry[which.min(oob_err$oob_error_rate)]]) 
mtry_x
min(oob_err$oob_error_rate)

#Apply it to the test data and get the confusion matrix and error rate:
rf.IberianQuarter=randomForest(Classification_name ~., data=spam_train,  importance=TRUE, mtry=mtry_x, ntree=ntree_x)

#predict on test set:
yhat <- predict(rf.IberianQuarter, spam_test, type = 'class')
y <- spam_test$Classification_name
table(y,yhat) 
mean(y != yhat)
#Find the 10 most important variables for the random Forest model.:
varImpPlot(rf.IberianQuarter, sort=TRUE, n.var=4) 

#the importance of every variable in classifying the data:
best <- as.data.frame(round(importance(rf.IberianQuarter), 2)) # get importance values
top <- best[order(-best$MeanDecreaseGini), , drop = FALSE] # sort highest first
top10 <- top[1:4, , drop=FALSE]
top10

#Month:
Data_Month_GEOIberian_Peninsula<-Data_Month_GEOIberian_Peninsula[!(Data_Month_GEOIberian_Peninsula$Classification_name=="3OHP"),]
Data_Month_GEOIberian_Peninsula<-Data_Month_GEOIberian_Peninsula[!(Data_Month_GEOIberian_Peninsula$Classification_name=="4MSO"),]
Data_Month_GEOIberian_Peninsula<-Data_Month_GEOIberian_Peninsula[!(Data_Month_GEOIberian_Peninsula$Classification_name=="Butenyl"),]


Data_Month_GEOIberian_Peninsula$WC2_BIO13<- as.numeric(Data_Month_GEOIberian_Peninsula$WC2_BIO13)
Data_Month_GEOIberian_Peninsula$SRTM_elevation<- as.numeric(Data_Month_GEOIberian_Peninsula$SRTM_elevation)
Data_Month_GEOIberian_Peninsula <- na.omit(Data_Month_GEOIberian_Peninsula) 
Data_Month_GEOIberian_Peninsula <- droplevels(Data_Month_GEOIberian_Peninsula) 

train <- sample(1:nrow(Data_Month_GEOIberian_Peninsula), nrow(Data_Month_GEOIberian_Peninsula)*2/3)
spam_train <- Data_Month_GEOIberian_Peninsula[train,]
spam_test <- Data_Month_GEOIberian_Peninsula[-train,]
summary(spam_train)
spam.bag <- randomForest(Classification_name~., data=spam_train, mtry=(dim(Data_Month_GEOIberian_Peninsula)[2]-1))
spam.bag

yhat=spam.bag$predicted
y=spam_train$Classification_name # training data
mean(y != yhat) # out of bag error rate for training data

#Apply it to the test data and get the confusion matrix and error rate:

yhat = predict(spam.bag, spam_test)
y = spam_test$Classification_name
table(y,yhat)
mean(y != yhat)

m = round(sqrt(dim(spam_train)[2]-1)) 
rm(oob_err)
for (i in c(500,1000,2000, 5000)){ # try three? different values for number of trees
  for (j in c(m-1, m, m+1)){ # tree three different values for number of predictors to sample
    set.seed(123)
    rf.spam=randomForest(Classification_name ~., data=spam_train,  mtry=j, ntree=i)
    # get oob error rate for training data
    yhat=rf.spam$predicted
    y=spam_train$Classification_name
    error_rate <- mean(y != yhat)
    if (exists('oob_err')==FALSE){
      oob_err = c(i,j,error_rate) # create initial data frame
    }
    else{
      oob_err = rbind(oob_err, c(i,j,error_rate)) # append to data frame of error rates, ntree, and mtry
    }
  }
}


oob_err <- as.data.frame(oob_err) 
names(oob_err) <- c('ntree', 'mtry', 'oob_error_rate') 
oob_err$mtry <- as.factor(oob_err$mtry) 
ggplot(oob_err, aes(x=ntree, y=oob_error_rate, group=mtry)) + geom_line(aes(color=mtry)) 

## get parameters for best tree:
ntree_x <- oob_err$ntree[which.min(oob_err$oob_error_rate)] 
ntree_x

# find mtry that minimizes oob_error_rate:
mtry_x <- as.numeric(levels(oob_err$mtry[which.min(oob_err$oob_error_rate)])[oob_err$mtry[which.min(oob_err$oob_error_rate)]]) 
mtry_x
min(oob_err$oob_error_rate)

#Apply it to the test data and get the confusion matrix and error rate:
rf.IberianMonth=randomForest(Classification_name ~., data=spam_train,  importance=TRUE, mtry=mtry_x, ntree=ntree_x)

#predict on test set:
yhat <- predict(rf.IberianMonth, spam_test, type = 'class')
y <- spam_test$Classification_name
table(y,yhat) 
mean(y != yhat)
#Find the 10 most important variables for the random Forest model.:
varImpPlot(rf.IberianMonth, sort=TRUE, n.var=4) 

#the importance of every variable in classifying the data:
best <- as.data.frame(round(importance(rf.IberianMonth), 2)) # get importance values
top <- best[order(-best$MeanDecreaseGini), , drop = FALSE] # sort highest first
top10 <- top[1:4, , drop=FALSE]
top10

#Europe:

#Quarter:
str(Data_Quarter_GEOEurope)

Data_Quarter_GEOEurope<-Data_Quarter_GEOEurope[!(Data_Quarter_GEOEurope$Classification_name=="4OHB"),]
Data_Quarter_GEOEurope<-Data_Quarter_GEOEurope[!(Data_Quarter_GEOEurope$Classification_name=="Butenyl"),]
Data_Quarter_GEOEurope<-Data_Quarter_GEOEurope[!(Data_Quarter_GEOEurope$Classification_name=="?"),]
Data_Quarter_GEOEurope<-Data_Quarter_GEOEurope[!(Data_Quarter_GEOEurope$Classification_name=="3MSO"),]



Data_Quarter_GEOEurope$WC2_BIO16<- as.numeric(Data_Quarter_GEOEurope$WC2_BIO16)
Data_Quarter_GEOEurope$SRTM_elevation<- as.numeric(Data_Quarter_GEOEurope$SRTM_elevation)
Data_Quarter_GEOEurope <- na.omit(Data_Quarter_GEOEurope) 
Data_Quarter_GEOEurope <- droplevels(Data_Quarter_GEOEurope) 

train <- sample(1:nrow(Data_Quarter_GEOEurope), nrow(Data_Quarter_GEOEurope)*2/3)
spam_train <- Data_Quarter_GEOEurope[train,]
spam_test <- Data_Quarter_GEOEurope[-train,]
summary(spam_train)
spam.bag <- randomForest(Classification_name~., data=spam_train, mtry=(dim(Data_Quarter_GEOEurope)[2]-1))
spam.bag

yhat=spam.bag$predicted
y=spam_train$Classification_name # training data
mean(y != yhat) # out of bag error rate for training data

#Apply it to the test data and get the confusion matrix and error rate:

yhat = predict(spam.bag, spam_test)
y = spam_test$Classification_name
table(y,yhat)
mean(y != yhat)

m = round(sqrt(dim(spam_train)[2]-1)) 
rm(oob_err)
for (i in c(500,1000,2000, 5000)){ # try three? different values for number of trees
  for (j in c(m-1, m, m+1)){ # tree three different values for number of predictors to sample
    set.seed(123)
    rf.spam=randomForest(Classification_name ~., data=spam_train,  mtry=j, ntree=i)
    # get oob error rate for training data
    yhat=rf.spam$predicted
    y=spam_train$Classification_name
    error_rate <- mean(y != yhat)
    if (exists('oob_err')==FALSE){
      oob_err = c(i,j,error_rate) # create initial data frame
    }
    else{
      oob_err = rbind(oob_err, c(i,j,error_rate)) # append to data frame of error rates, ntree, and mtry
    }
  }
}


oob_err <- as.data.frame(oob_err) 
names(oob_err) <- c('ntree', 'mtry', 'oob_error_rate') 
oob_err$mtry <- as.factor(oob_err$mtry) 
ggplot(oob_err, aes(x=ntree, y=oob_error_rate, group=mtry)) + geom_line(aes(color=mtry)) 

## get parameters for best tree:
ntree_x <- oob_err$ntree[which.min(oob_err$oob_error_rate)] 
ntree_x

# find mtry that minimizes oob_error_rate:
mtry_x <- as.numeric(levels(oob_err$mtry[which.min(oob_err$oob_error_rate)])[oob_err$mtry[which.min(oob_err$oob_error_rate)]]) 
mtry_x
min(oob_err$oob_error_rate)

#Apply it to the test data and get the confusion matrix and error rate:
rf.EuropeQuarter=randomForest(Classification_name ~., data=spam_train,  importance=TRUE, mtry=mtry_x, ntree=ntree_x)

#predict on test set:
yhat <- predict(rf.EuropeQuarter, spam_test, type = 'class')
y <- spam_test$Classification_name
table(y,yhat) 
mean(y != yhat)
#Find the 10 most important variables for the random Forest model.:
varImpPlot(rf.EuropeQuarter, sort=TRUE, n.var=4) 

#the importance of every variable in classifying the data:
best <- as.data.frame(round(importance(rf.EuropeQuarter), 2)) # get importance values
top <- best[order(-best$MeanDecreaseGini), , drop = FALSE] # sort highest first
top10 <- top[1:4, , drop=FALSE]
top10

#Month:
str(Data_Month_GEOEurope)

Data_Month_GEOEurope<-Data_Month_GEOEurope[!(Data_Month_GEOEurope$Classification_name=="4OHB"),]
Data_Month_GEOEurope<-Data_Month_GEOEurope[!(Data_Month_GEOEurope$Classification_name=="Butenyl"),]
Data_Month_GEOEurope<-Data_Month_GEOEurope[!(Data_Month_GEOEurope$Classification_name=="?"),]
Data_Month_GEOEurope<-Data_Month_GEOEurope[!(Data_Month_GEOEurope$Classification_name=="3MSO"),]


Data_Month_GEOEurope$WC2_BIO13<- as.numeric(Data_Month_GEOEurope$WC2_BIO13)
Data_Month_GEOEurope$SRTM_elevation<- as.numeric(Data_Month_GEOEurope$SRTM_elevation)
Data_Month_GEOEurope <- na.omit(Data_Month_GEOEurope)
Data_Month_GEOEurope <- droplevels(Data_Month_GEOEurope)


train <- sample(1:nrow(Data_Month_GEOEurope), nrow(Data_Month_GEOEurope)*2/3)
spam_train <- Data_Month_GEOEurope[train,]
spam_test <- Data_Month_GEOEurope[-train,]
summary(spam_train)
spam.bag <- randomForest(Classification_name~., data=spam_train, mtry=(dim(Data_Month_GEOEurope)[2]-1))
spam.bag

yhat=spam.bag$predicted
y=spam_train$Classification_name # training data
mean(y != yhat) # out of bag error rate for training data

#Apply it to the test data and get the confusion matrix and error rate:

yhat = predict(spam.bag, spam_test)
y = spam_test$Classification_name
table(y,yhat)
mean(y != yhat)

m = round(sqrt(dim(spam_train)[2]-1)) 
rm(oob_err)
for (i in c(500,1000,2000, 5000)){ # try three? different values for number of trees
  for (j in c(m-1, m, m+1)){ # tree three different values for number of predictors to sample
    set.seed(123)
    rf.spam=randomForest(Classification_name ~., data=spam_train,  mtry=j, ntree=i)
    # get oob error rate for training data
    yhat=rf.spam$predicted
    y=spam_train$Classification_name
    error_rate <- mean(y != yhat)
    if (exists('oob_err')==FALSE){
      oob_err = c(i,j,error_rate) # create initial data frame
    }
    else{
      oob_err = rbind(oob_err, c(i,j,error_rate)) # append to data frame of error rates, ntree, and mtry
    }
  }
}


oob_err <- as.data.frame(oob_err) 
names(oob_err) <- c('ntree', 'mtry', 'oob_error_rate') 
oob_err$mtry <- as.factor(oob_err$mtry) 
ggplot(oob_err, aes(x=ntree, y=oob_error_rate, group=mtry)) + geom_line(aes(color=mtry)) 

## get parameters for best tree:
ntree_x <- oob_err$ntree[which.min(oob_err$oob_error_rate)] 
ntree_x

# find mtry that minimizes oob_error_rate:
mtry_x <- as.numeric(levels(oob_err$mtry[which.min(oob_err$oob_error_rate)])[oob_err$mtry[which.min(oob_err$oob_error_rate)]]) 
mtry_x
min(oob_err$oob_error_rate)

#Apply it to the test data and get the confusion matrix and error rate:
rf.EuropeMonth=randomForest(Classification_name ~., data=spam_train,  importance=TRUE, mtry=mtry_x, ntree=ntree_x)

#predict on test set:
yhat <- predict(rf.EuropeMonth, spam_test, type = 'class')
y <- spam_test$Classification_name
table(y,yhat) 
mean(y != yhat)
#Find the 10 most important variables for the random Forest model.:
varImpPlot(rf.EuropeMonth, sort=TRUE, n.var=4) 

#the importance of every variable in classifying the data:
best <- as.data.frame(round(importance(rf.EuropeMonth), 2)) # get importance values
top <- best[order(-best$MeanDecreaseGini), , drop = FALSE] # sort highest first
top10 <- top[1:4, , drop=FALSE]
top10

#adding our grouping:

#Grouping by geography:
Accessions_data$GEO <- NA
Accessions_data$GEO[which(Accessions_data$Country == "ESP")] <- "Iberian_Peninsula"
Accessions_data$GEO[which(Accessions_data$Country == "POR")] <- "Iberian_Peninsula"
Accessions_data$GEO[which(Accessions_data$Country == "ITA")] <- "Italy_Balkan"
Accessions_data$GEO[which(Accessions_data$Country == "BUL")] <- "Italy_Balkan"
Accessions_data$GEO[which(Accessions_data$Country == "CRO")] <- "Italy_Balkan"
Accessions_data$GEO[which(Accessions_data$Country == "GRC")] <- "Italy_Balkan"
Accessions_data$GEO[which(Accessions_data$Country == "SRB")] <- "Italy_Balkan"
Accessions_data$GEO[which(Accessions_data$Country == "ROU")] <- "Italy_Balkan"
Accessions_data$GEO[which(Accessions_data$Country == "LBN")] <- "Italy_Balkan"
Accessions_data$GEO[which(Accessions_data$Country == "FRA")] <- "France"
Accessions_data$GEO[which(Accessions_data$Country == "UK")] <- "UK"
Accessions_data$GEO[which(Accessions_data$Country == "IRL")] <- "UK"
Accessions_data$GEO[is.na(Accessions_data$GEO)] <- "Germany_and_East"

Accessions_data<-Accessions_data[!(Accessions_data$Classification_name=="4OHB"),]
Accessions_data<-Accessions_data[!(Accessions_data$Classification_name=="Butenyl"),]
Accessions_data<-Accessions_data[!(Accessions_data$Classification_name=="?"),]
Accessions_data<-Accessions_data[!(Accessions_data$Classification_name=="3MSO"),]
Accessions_data<-droplevels(Accessions_data)

colnames(Accessions_data)
Enviromental_Quarter <-Enviromental_data[,c(1,2,91,115, 42)]
Enviromental_Month <-Enviromental_data[,c(1,2,88,81, 42)]
Accessions_1 <- Accessions_data[,c(1,20,31)]
Accessions_2 <- Accessions_data[,c(20, 15:17,31)]
Data_Quarter_1 <- merge(Accessions_1, Enviromental_Quarter, by="CS")
Data_Quarter <-Data_Quarter_1[,-c(1)]
Data_Month_1<- merge(Accessions_1, Enviromental_Month, by="CS")
Data_Month <- Data_Month_1[,-c(1)]

#All data:
#All, quarter:
#2/3 training and 1/3 test data:
str(Data_Quarter)

Data_Quarter$WC2_BIO16<- as.numeric(Data_Quarter$WC2_BIO16)
Data_Quarter$SRTM_elevation<- as.numeric(Data_Quarter$SRTM_elevation)
Data_Quarter$GEO<- as.factor(Data_Quarter$GEO)
Data_Quarter <- na.omit(Data_Quarter) 

train <- sample(1:nrow(Data_Quarter), nrow(Data_Quarter)*2/3)
spam_train <- Data_Quarter[train,]
spam_test <- Data_Quarter[-train,]
summary(spam_train)
spam.bag <- randomForest(Classification_name~., data=spam_train, mtry=(dim(Data_Quarter)[2]-1))
spam.bag

yhat=spam.bag$predicted
y=spam_train$Classification_name # training data
mean(y != yhat) # out of bag error rate for training data

#Apply it to the test data and get the confusion matrix and error rate:

yhat = predict(spam.bag, spam_test)
y = spam_test$Classification_name
table(y,yhat)
mean(y != yhat)

m = round(sqrt(dim(spam_train)[2]-1)) 
rm(oob_err)
for (i in c(500,1000,2000, 5000)){ # try three? different values for number of trees
  for (j in c(m-1, m, m+1)){ # tree three different values for number of predictors to sample
    set.seed(123)
    rf.spam=randomForest(Classification_name ~., data=spam_train,  mtry=j, ntree=i)
    # get oob error rate for training data
    yhat=rf.spam$predicted
    y=spam_train$Classification_name
    error_rate <- mean(y != yhat)
    if (exists('oob_err')==FALSE){
      oob_err = c(i,j,error_rate) # create initial data frame
    }
    else{
      oob_err = rbind(oob_err, c(i,j,error_rate)) # append to data frame of error rates, ntree, and mtry
    }
  }
}


oob_err <- as.data.frame(oob_err) 
names(oob_err) <- c('ntree', 'mtry', 'oob_error_rate') 
oob_err$mtry <- as.factor(oob_err$mtry) 
ggplot(oob_err, aes(x=ntree, y=oob_error_rate, group=mtry)) + geom_line(aes(color=mtry)) 

## get parameters for best tree:
ntree_x <- oob_err$ntree[which.min(oob_err$oob_error_rate)] 
ntree_x

# find mtry that minimizes oob_error_rate:
mtry_x <- as.numeric(levels(oob_err$mtry[which.min(oob_err$oob_error_rate)])[oob_err$mtry[which.min(oob_err$oob_error_rate)]]) 
mtry_x
min(oob_err$oob_error_rate)

#Apply it to the test data and get the confusion matrix and error rate:
rf.All_quarter=randomForest(Classification_name ~., data=spam_train,  importance=TRUE, mtry=mtry_x, ntree=ntree_x)

#predict on test set:
yhat <- predict(rf.All_quarter, spam_test, type = 'class')
y <- spam_test$Classification_name
table(y,yhat) 
mean(y != yhat)
#Find the 10 most important variables for the random Forest model.:
varImpPlot(rf.All_quarter, sort=TRUE, n.var=5) 

#the importance of every variable in classifying the data:
best <- as.data.frame(round(importance(rf.All_quarter), 2)) # get importance values
top <- best[order(-best$MeanDecreaseGini), , drop = FALSE] # sort highest first
top10 <- top[1:5, , drop=FALSE]
top10


#All, Month:
#2/3 training and 1/3 test data:
str(Data_Month)

Data_Month$WC2_BIO13<- as.numeric(Data_Month$WC2_BIO13)
Data_Month$SRTM_elevation<- as.numeric(Data_Month$SRTM_elevation)
Data_Month$GEO<- as.factor(Data_Month$GEO)
Data_Month <- na.omit(Data_Month) 

train <- sample(1:nrow(Data_Month), nrow(Data_Month)*2/3)
spam_train <- Data_Month[train,]
spam_test <- Data_Month[-train,]
summary(spam_train)
spam.bag <- randomForest(Classification_name~., data=spam_train, mtry=(dim(Data_Month)[2]-1))
spam.bag

yhat=spam.bag$predicted
y=spam_train$Classification_name # training data
mean(y != yhat) # out of bag error rate for training data

#Apply it to the test data and get the confusion matrix and error rate:

yhat = predict(spam.bag, spam_test)
y = spam_test$Classification_name
table(y,yhat)
mean(y != yhat)
rm(oob_err)
m = round(sqrt(dim(spam_train)[2]-1)) 

for (i in c(500,1000,2000, 5000)){ # try three? different values for number of trees
  for (j in c(m-1, m, m+1)){ # tree three different values for number of predictors to sample
    set.seed(123)
    rf.spam=randomForest(Classification_name ~., data=spam_train,  mtry=j, ntree=i)
    # get oob error rate for training data
    yhat=rf.spam$predicted
    y=spam_train$Classification_name
    error_rate <- mean(y != yhat)
    if (exists('oob_err')==FALSE){
      oob_err = c(i,j,error_rate) # create initial data frame
    }
    else{
      oob_err = rbind(oob_err, c(i,j,error_rate)) # append to data frame of error rates, ntree, and mtry
    }
  }
}


oob_err <- as.data.frame(oob_err) 
names(oob_err) <- c('ntree', 'mtry', 'oob_error_rate') 
oob_err$mtry <- as.factor(oob_err$mtry) 
ggplot(oob_err, aes(x=ntree, y=oob_error_rate, group=mtry)) + geom_line(aes(color=mtry)) 

## get parameters for best tree:
ntree_x <- oob_err$ntree[which.min(oob_err$oob_error_rate)] 
ntree_x

# find mtry that minimizes oob_error_rate:
mtry_x <- as.numeric(levels(oob_err$mtry[which.min(oob_err$oob_error_rate)])[oob_err$mtry[which.min(oob_err$oob_error_rate)]]) 
mtry_x
min(oob_err$oob_error_rate)

#Apply it to the test data and get the confusion matrix and error rate:
rf.All_month=randomForest(Classification_name ~., data=spam_train,  importance=TRUE, mtry=mtry_x, ntree=ntree_x)

#predict on test set:
yhat <- predict(rf.All_month, spam_test, type = 'class')
y <- spam_test$Classification_name
table(y,yhat) 
mean(y != yhat)
#Find the 10 most important variables for the random Forest model.:
varImpPlot(rf.All_month, sort=TRUE, n.var=5) 

#the importance of every variable in classifying the data:
best <- as.data.frame(round(importance(rf.All_month), 2)) # get importance values
top <- best[order(-best$MeanDecreaseGini), , drop = FALSE] # sort highest first
top10 <- top[1:5, , drop=FALSE]
top10

#Subseting the data:

Accessions_data$GEO <- NA
Accessions_data$GEO[which(Accessions_data$Country == "ESP")] <- "Iberian_Peninsula"
Accessions_data$GEO[which(Accessions_data$Country == "POR")] <- "Iberian_Peninsula"
Accessions_data$GEO[is.na(Accessions_data$GEO)] <- "Europe"

Accessions_data$GEO <- as.factor(Accessions_data$GEO)



colnames(Accessions_data)

Enviromental_Quarter <-Enviromental_data[,c(1,2,91,115, 42)]
Enviromental_Month <-Enviromental_data[,c(1,2,88,81, 42)]
Accessions_1 <- Accessions_data[,c(1,20,31)]
Accessions_2 <- Accessions_data[,c(20, 15:17,31)]
Data_Quarter_1 <- merge(Accessions_1, Enviromental_Quarter, by="CS")
Data_Quarter <-Data_Quarter_1[,-c(1)]
Data_Month_1<- merge(Accessions_1, Enviromental_Month, by="CS")
Data_Month <- Data_Month_1[,-c(1)]

Data_Quarter_GEO <- split(Data_Quarter, Data_Quarter$GEO)
list2env(Data_Quarter_GEO, envir=.GlobalEnv)
rm(Data_Quarter_GEO)
Data_Quarter_GEOIberian_Peninsula <- droplevels.data.frame(Iberian_Peninsula)
Data_Quarter_GEOEurope <- droplevels.data.frame(Europe)

Data_Month_GEO <- split(Data_Month, Data_Month$GEO)
list2env(Data_Month_GEO, envir=.GlobalEnv)
rm(Data_Month_GEO)
Data_Month_GEOIberian_Peninsula <- droplevels.data.frame(Iberian_Peninsula)
Data_Month_GEOEurope <- droplevels.data.frame(Europe)

#Iberian Peninsula:
#Quarter:
str(Data_Quarter_GEOIberian_Peninsula)
Data_Quarter_GEOIberian_Peninsula$WC2_BIO16<- as.numeric(Data_Quarter_GEOIberian_Peninsula$WC2_BIO16)
Data_Quarter_GEOIberian_Peninsula$SRTM_elevation<- as.numeric(Data_Quarter_GEOIberian_Peninsula$SRTM_elevation)
Data_Quarter_GEOIberian_Peninsula <- na.omit(Data_Quarter_GEOIberian_Peninsula) 

train <- sample(1:nrow(Data_Quarter_GEOIberian_Peninsula), nrow(Data_Quarter_GEOIberian_Peninsula)*2/3)
spam_train <- Data_Quarter_GEOIberian_Peninsula[train,]
spam_test <- Data_Quarter_GEOIberian_Peninsula[-train,]
summary(spam_train)
spam.bag <- randomForest(Classification_name~., data=spam_train, mtry=(dim(Data_Quarter_GEOIberian_Peninsula)[2]-1))
spam.bag

yhat=spam.bag$predicted
y=spam_train$Classification_name # training data
mean(y != yhat) # out of bag error rate for training data

#Apply it to the test data and get the confusion matrix and error rate:

yhat = predict(spam.bag, spam_test)
y = spam_test$Classification_name
table(y,yhat)
mean(y != yhat)

m = round(sqrt(dim(spam_train)[2]-1)) 
rm(oob_err)
for (i in c(500,1000,2000, 5000)){ # try three? different values for number of trees
  for (j in c(m-1, m, m+1)){ # tree three different values for number of predictors to sample
    set.seed(123)
    rf.spam=randomForest(Classification_name ~., data=spam_train,  mtry=j, ntree=i)
    # get oob error rate for training data
    yhat=rf.spam$predicted
    y=spam_train$Classification_name
    error_rate <- mean(y != yhat)
    if (exists('oob_err')==FALSE){
      oob_err = c(i,j,error_rate) # create initial data frame
    }
    else{
      oob_err = rbind(oob_err, c(i,j,error_rate)) # append to data frame of error rates, ntree, and mtry
    }
  }
}


oob_err <- as.data.frame(oob_err) 
names(oob_err) <- c('ntree', 'mtry', 'oob_error_rate') 
oob_err$mtry <- as.factor(oob_err$mtry) 
ggplot(oob_err, aes(x=ntree, y=oob_error_rate, group=mtry)) + geom_line(aes(color=mtry)) 

## get parameters for best tree:
ntree_x <- oob_err$ntree[which.min(oob_err$oob_error_rate)] 
ntree_x

# find mtry that minimizes oob_error_rate:
mtry_x <- as.numeric(levels(oob_err$mtry[which.min(oob_err$oob_error_rate)])[oob_err$mtry[which.min(oob_err$oob_error_rate)]]) 
mtry_x
min(oob_err$oob_error_rate)

#Apply it to the test data and get the confusion matrix and error rate:
rf.IberianQuarter=randomForest(Classification_name ~., data=spam_train,  importance=TRUE, mtry=mtry_x, ntree=ntree_x)

#predict on test set:
yhat <- predict(rf.IberianQuarter, spam_test, type = 'class')
y <- spam_test$Classification_name
table(y,yhat) 
mean(y != yhat)
#Find the 10 most important variables for the random Forest model.:
varImpPlot(rf.IberianQuarter, sort=TRUE, n.var=4) 

#the importance of every variable in classifying the data:
best <- as.data.frame(round(importance(rf.IberianQuarter), 2)) # get importance values
top <- best[order(-best$MeanDecreaseGini), , drop = FALSE] # sort highest first
top10 <- top[1:4, , drop=FALSE]
top10

#Month:
Data_Month_GEOIberian_Peninsula$WC2_BIO13<- as.numeric(Data_Month_GEOIberian_Peninsula$WC2_BIO13)
Data_Month_GEOIberian_Peninsula$SRTM_elevation<- as.numeric(Data_Month_GEOIberian_Peninsula$SRTM_elevation)
Data_Month_GEOIberian_Peninsula <- na.omit(Data_Month_GEOIberian_Peninsula) 


train <- sample(1:nrow(Data_Month_GEOIberian_Peninsula), nrow(Data_Month_GEOIberian_Peninsula)*2/3)
spam_train <- Data_Month_GEOIberian_Peninsula[train,]
spam_test <- Data_Month_GEOIberian_Peninsula[-train,]
summary(spam_train)
spam.bag <- randomForest(Classification_name~., data=spam_train, mtry=(dim(Data_Month_GEOIberian_Peninsula)[2]-1))
spam.bag

yhat=spam.bag$predicted
y=spam_train$Classification_name # training data
mean(y != yhat) # out of bag error rate for training data

#Apply it to the test data and get the confusion matrix and error rate:

yhat = predict(spam.bag, spam_test)
y = spam_test$Classification_name
table(y,yhat)
mean(y != yhat)

m = round(sqrt(dim(spam_train)[2]-1)) 
rm(oob_err)
for (i in c(500,1000,2000, 5000)){ # try three? different values for number of trees
  for (j in c(m-1, m, m+1)){ # tree three different values for number of predictors to sample
    set.seed(123)
    rf.spam=randomForest(Classification_name ~., data=spam_train,  mtry=j, ntree=i)
    # get oob error rate for training data
    yhat=rf.spam$predicted
    y=spam_train$Classification_name
    error_rate <- mean(y != yhat)
    if (exists('oob_err')==FALSE){
      oob_err = c(i,j,error_rate) # create initial data frame
    }
    else{
      oob_err = rbind(oob_err, c(i,j,error_rate)) # append to data frame of error rates, ntree, and mtry
    }
  }
}


oob_err <- as.data.frame(oob_err) 
names(oob_err) <- c('ntree', 'mtry', 'oob_error_rate') 
oob_err$mtry <- as.factor(oob_err$mtry) 
ggplot(oob_err, aes(x=ntree, y=oob_error_rate, group=mtry)) + geom_line(aes(color=mtry)) 

## get parameters for best tree:
ntree_x <- oob_err$ntree[which.min(oob_err$oob_error_rate)] 
ntree_x

# find mtry that minimizes oob_error_rate:
mtry_x <- as.numeric(levels(oob_err$mtry[which.min(oob_err$oob_error_rate)])[oob_err$mtry[which.min(oob_err$oob_error_rate)]]) 
mtry_x
min(oob_err$oob_error_rate)

#Apply it to the test data and get the confusion matrix and error rate:
rf.IberianMonth=randomForest(Classification_name ~., data=spam_train,  importance=TRUE, mtry=mtry_x, ntree=ntree_x)

#predict on test set:
yhat <- predict(rf.IberianMonth, spam_test, type = 'class')
y <- spam_test$Classification_name
table(y,yhat) 
mean(y != yhat)
#Find the 10 most important variables for the random Forest model.:
varImpPlot(rf.IberianMonth, sort=TRUE, n.var=4) 

#the importance of every variable in classifying the data:
best <- as.data.frame(round(importance(rf.IberianMonth), 2)) # get importance values
top <- best[order(-best$MeanDecreaseGini), , drop = FALSE] # sort highest first
top10 <- top[1:4, , drop=FALSE]
top10

#Europe:

#Quarter:
str(Data_Quarter_GEOEurope)
Data_Quarter_GEOEurope$WC2_BIO16<- as.numeric(Data_Quarter_GEOEurope$WC2_BIO16)
Data_Quarter_GEOEurope$SRTM_elevation<- as.numeric(Data_Quarter_GEOEurope$SRTM_elevation)
Data_Quarter_GEOEurope <- na.omit(Data_Quarter_GEOEurope) 

train <- sample(1:nrow(Data_Quarter_GEOEurope), nrow(Data_Quarter_GEOEurope)*2/3)
spam_train <- Data_Quarter_GEOEurope[train,]
spam_test <- Data_Quarter_GEOEurope[-train,]
summary(spam_train)
spam.bag <- randomForest(Classification_name~., data=spam_train, mtry=(dim(Data_Quarter_GEOEurope)[2]-1))
spam.bag

yhat=spam.bag$predicted
y=spam_train$Classification_name # training data
mean(y != yhat) # out of bag error rate for training data

#Apply it to the test data and get the confusion matrix and error rate:

yhat = predict(spam.bag, spam_test)
y = spam_test$Classification_name
table(y,yhat)
mean(y != yhat)

m = round(sqrt(dim(spam_train)[2]-1)) 
rm(oob_err)
for (i in c(500,1000,2000, 5000)){ # try three? different values for number of trees
  for (j in c(m-1, m, m+1)){ # tree three different values for number of predictors to sample
    set.seed(123)
    rf.spam=randomForest(Classification_name ~., data=spam_train,  mtry=j, ntree=i)
    # get oob error rate for training data
    yhat=rf.spam$predicted
    y=spam_train$Classification_name
    error_rate <- mean(y != yhat)
    if (exists('oob_err')==FALSE){
      oob_err = c(i,j,error_rate) # create initial data frame
    }
    else{
      oob_err = rbind(oob_err, c(i,j,error_rate)) # append to data frame of error rates, ntree, and mtry
    }
  }
}


oob_err <- as.data.frame(oob_err) 
names(oob_err) <- c('ntree', 'mtry', 'oob_error_rate') 
oob_err$mtry <- as.factor(oob_err$mtry) 
ggplot(oob_err, aes(x=ntree, y=oob_error_rate, group=mtry)) + geom_line(aes(color=mtry)) 

## get parameters for best tree:
ntree_x <- oob_err$ntree[which.min(oob_err$oob_error_rate)] 
ntree_x

# find mtry that minimizes oob_error_rate:
mtry_x <- as.numeric(levels(oob_err$mtry[which.min(oob_err$oob_error_rate)])[oob_err$mtry[which.min(oob_err$oob_error_rate)]]) 
mtry_x
min(oob_err$oob_error_rate)

#Apply it to the test data and get the confusion matrix and error rate:
rf.EuropeQuarter=randomForest(Classification_name ~., data=spam_train,  importance=TRUE, mtry=mtry_x, ntree=ntree_x)

#predict on test set:
yhat <- predict(rf.EuropeQuarter, spam_test, type = 'class')
y <- spam_test$Classification_name
table(y,yhat) 
mean(y != yhat)
#Find the 10 most important variables for the random Forest model.:
varImpPlot(rf.EuropeQuarter, sort=TRUE, n.var=4) 

#the importance of every variable in classifying the data:
best <- as.data.frame(round(importance(rf.EuropeQuarter), 2)) # get importance values
top <- best[order(-best$MeanDecreaseGini), , drop = FALSE] # sort highest first
top10 <- top[1:4, , drop=FALSE]
top10

#Month:
str(Data_Month_GEOEurope)
Data_Month_GEOEurope$WC2_BIO13<- as.numeric(Data_Month_GEOEurope$WC2_BIO13)
Data_Month_GEOEurope$SRTM_elevation<- as.numeric(Data_Month_GEOEurope$SRTM_elevation)
Data_Month_GEOEurope <- na.omit(Data_Month_GEOEurope) 

train <- sample(1:nrow(Data_Month_GEOEurope), nrow(Data_Month_GEOEurope)*2/3)
spam_train <- Data_Month_GEOEurope[train,]
spam_test <- Data_Month_GEOEurope[-train,]
summary(spam_train)
spam.bag <- randomForest(Classification_name~., data=spam_train, mtry=(dim(Data_Month_GEOEurope)[2]-1))
spam.bag

yhat=spam.bag$predicted
y=spam_train$Classification_name # training data
mean(y != yhat) # out of bag error rate for training data

#Apply it to the test data and get the confusion matrix and error rate:

yhat = predict(spam.bag, spam_test)
y = spam_test$Classification_name
table(y,yhat)
mean(y != yhat)

m = round(sqrt(dim(spam_train)[2]-1)) 
rm(oob_err)
for (i in c(500,1000,2000, 5000)){ # try three? different values for number of trees
  for (j in c(m-1, m, m+1)){ # tree three different values for number of predictors to sample
    set.seed(123)
    rf.spam=randomForest(Classification_name ~., data=spam_train,  mtry=j, ntree=i)
    # get oob error rate for training data
    yhat=rf.spam$predicted
    y=spam_train$Classification_name
    error_rate <- mean(y != yhat)
    if (exists('oob_err')==FALSE){
      oob_err = c(i,j,error_rate) # create initial data frame
    }
    else{
      oob_err = rbind(oob_err, c(i,j,error_rate)) # append to data frame of error rates, ntree, and mtry
    }
  }
}


oob_err <- as.data.frame(oob_err) 
names(oob_err) <- c('ntree', 'mtry', 'oob_error_rate') 
oob_err$mtry <- as.factor(oob_err$mtry) 
ggplot(oob_err, aes(x=ntree, y=oob_error_rate, group=mtry)) + geom_line(aes(color=mtry)) 

## get parameters for best tree:
ntree_x <- oob_err$ntree[which.min(oob_err$oob_error_rate)] 
ntree_x

# find mtry that minimizes oob_error_rate:
mtry_x <- as.numeric(levels(oob_err$mtry[which.min(oob_err$oob_error_rate)])[oob_err$mtry[which.min(oob_err$oob_error_rate)]]) 
mtry_x
min(oob_err$oob_error_rate)

#Apply it to the test data and get the confusion matrix and error rate:
rf.EuropeMonth=randomForest(Classification_name ~., data=spam_train,  importance=TRUE, mtry=mtry_x, ntree=ntree_x)

#predict on test set:
yhat <- predict(rf.EuropeMonth, spam_test, type = 'class')
y <- spam_test$Classification_name
table(y,yhat) 
mean(y != yhat)
#Find the 10 most important variables for the random Forest model.:
varImpPlot(rf.EuropeMonth, sort=TRUE, n.var=4) 

#the importance of every variable in classifying the data:
best <- as.data.frame(round(importance(rf.EuropeMonth), 2)) # get importance values
top <- best[order(-best$MeanDecreaseGini), , drop = FALSE] # sort highest first
top10 <- top[1:4, , drop=FALSE]
top10


#Parameters:
str(spam_train)
summary(spam_train)
    
set.seed(1234)
model2 <- randomForest(formula = as.factor(Classification_name) ~ MAM+GSOH+AOP,
                       ntree=1000, data = Accessions_2)
print(model2)
round(importance(model2), 2)
#Quarter:

Data_Quarter$WC2_BIO16 <- as.numeric(Data_Quarter$WC2_BIO16)
str(Data_Quarter)
summary(Data_Quarter)

Data_Quarter_1 <-Data_Quarter[-c(279, 379:381,516,517, 530, 778:781,822, 829:831 ),]
set.seed(1234)
model <- randomForest(formula = as.factor(Classification_name) ~ group+ WC2_BIO16+ CHELSA_BIO11
                      +group* WC2_BIO16+ WC2_BIO16* CHELSA_BIO11+ group*CHELSA_BIO11+ group* WC2_BIO16* CHELSA_BIO11,
                      ntree=1000, data = Data_Quarter_1)
print(model)
round(importance(model), 2)

#Month:
Data_Month$WC2_BIO13 <- as.numeric(Data_Month$WC2_BIO13)
str(Data_Month)
summary(Data_Quarter)

Data_Month_1 <-Data_Month[-c(279, 379:381,516,517, 530, 778:781,822, 829:831 ),]
set.seed(1234)
model1 <- randomForest(formula = as.factor(Classification_name) ~ group+ WC2_BIO13+ WC2_BIO6+
                         group*WC2_BIO13 +WC2_BIO13* WC2_BIO6+ group+ WC2_BIO6+ group* WC2_BIO13* WC2_BIO6,
                      ntree=1000, data = Data_Month_1)
print(model1)
round(importance(model1), 2)

#Separating Iberian peninsula:
Accessions_data$GEO <- NA
Accessions_data$GEO[which(Accessions_data$Country == "ESP")] <- "Iberian_Peninsula"
Accessions_data$GEO[which(Accessions_data$Country == "POR")] <- "Iberian_Peninsula"
Accessions_data$GEO[which(Accessions_data$Country == "MAR")] <- "Iberian_Peninsula"
Accessions_data$GEO[which(Accessions_data$Country == "USA")] <- "USA"
Accessions_data$GEO[which(Accessions_data$Country == "CAN")] <- "USA"
Accessions_data$GEO[which(Accessions_data$Country == "ITA")] <- "Italy_Balkan"
Accessions_data$GEO[which(Accessions_data$Country == "BUL")] <- "Italy_Balkan"
Accessions_data$GEO[which(Accessions_data$Country == "CRO")] <- "Italy_Balkan"
Accessions_data$GEO[which(Accessions_data$Country == "GRC")] <- "Italy_Balkan"
Accessions_data$GEO[which(Accessions_data$Country == "SRB")] <- "Italy_Balkan"
Accessions_data$GEO[which(Accessions_data$Country == "ROU")] <- "Italy_Balkan"
Data_1$GEO[which(Data_1$Country == "LBN")] <- "Italy_Balkan"
Accessions_data$GEO[is.na(Accessions_data$GEO)] <- "Europe"

Iberian_data <- Accessions_data[ which(Accessions_data$GEO=='Iberian_Peninsula'),]
Europe_data <- Accessions_data[ which(Accessions_data$GEO=='Europe'),]
Italy_Balkan_data <- Accessions_data[ which(Accessions_data$GEO=='Italy_Balkan'),]

Iberian_1 <- Iberian_data[,c(1,20, 15:17)]
Europe_1 <- Europe_data[,c(1,20, 15:17)]
Italy_Balkan_1 <- Italy_Balkan_data[,c(1,20, 15:17)]
  
Iberian_Quarter <- merge(Iberian_1, Enviromental_Quarter, by="CS")
Iberian_Month<- merge(Iberian_1, Enviromental_Month, by="CS")

Europe_Quarter <- merge(Europe_1, Enviromental_Quarter, by="CS")
Europe_Month<- merge(Europe_1, Enviromental_Month, by="CS")

Italy_Balkan_Quarter <- merge(Italy_Balkan_1, Enviromental_Quarter, by="CS")
Italy_Balkan_Month<- merge(Italy_Balkan_1, Enviromental_Month, by="CS")

#All with GEO (until line 118):
colnames(Accessions_data)

Accessions_3 <- Accessions_data[,c(1,20,31)]
Data_all_Quarter <- merge(Accessions_3, Enviromental_Quarter, by="CS")
Data_all_Month<- merge(Accessions_3, Enviromental_Month, by="CS")

Data_all_Quarter$WC2_BIO16 <- as.numeric(Data_all_Quarter$WC2_BIO16)
Data_all_Quarter$GEO <- as.factor(Data_all_Quarter$GEO)
str(Data_all_Quarter_1)
summary(Data_Quarter)

Data_all_Quarter_1 <-Data_all_Quarter[-c(279, 379:381,516,517, 530, 778:781,822, 829:831),]
set.seed(1234)
Data_all_Quarter_1 <- droplevels(Data_all_Quarter_1)
model9 <- randomForest(formula = as.factor(Classification_name) ~ group+ WC2_BIO16+ CHELSA_BIO11+
                         group* WC2_BIO16+ WC2_BIO16* CHELSA_BIO11+ group*CHELSA_BIO11+ group* WC2_BIO16* CHELSA_BIO11+ GEO,
                       ntree=1000, data = Data_all_Quarter_1)
print(model9)
round(importance(model9), 2)
pred <- predict(model3, Iberian_Quarter_1, type = "class")
# Checking classification accuracy
table(pred, Iberian_Quarter_1$Classification_name)  

varImpPlot(model3)        




Iberian_Month$WC2_BIO13 <- as.numeric(Iberian_Month$WC2_BIO13)
Iberian_Month_1 <-Iberian_Month[-c(87),]
Iberian_Month_1 <- droplevels(Iberian_Month_1)
str(Iberian_Month)
summary(Data_Quarter)


set.seed(1234)
model4 <- randomForest(formula = as.factor(Classification_name) ~ group+ WC2_BIO13+ WC2_BIO6+
                         group* WC2_BIO13+ WC2_BIO13+ WC2_BIO6 + group* WC2_BIO6+ group*WC2_BIO13* WC2_BIO6,
                       ntree=1000, data = Iberian_Month_1)
print(model4)
round(importance(model4), 2)



Iberian_Quarter$WC2_BIO16 <- as.numeric(Iberian_Quarter$WC2_BIO16)
str(Iberian_Quarter_1)
summary(Data_Quarter)

Iberian_Quarter_1 <-Iberian_Quarter[-c(87),]
set.seed(1234)
Iberian_Quarter_1 <- droplevels(Iberian_Quarter_1)
model3 <- randomForest(formula = as.factor(Classification_name) ~ group+ WC2_BIO16+ CHELSA_BIO11+
                         group* WC2_BIO16+ WC2_BIO16* CHELSA_BIO11+ group*CHELSA_BIO11+ group* WC2_BIO16* CHELSA_BIO11,
                       ntree=1000, data = Iberian_Quarter_1)
print(model3)
round(importance(model3), 2)
pred <- predict(model3, Iberian_Quarter_1, type = "class")
# Checking classification accuracy
table(pred, Iberian_Quarter_1$Classification_name)  
    
varImpPlot(model3)        




Iberian_Month$WC2_BIO13 <- as.numeric(Iberian_Month$WC2_BIO13)
Iberian_Month_1 <-Iberian_Month[-c(87),]
Iberian_Month_1 <- droplevels(Iberian_Month_1)
str(Iberian_Month)
summary(Data_Quarter)


set.seed(1234)
model4 <- randomForest(formula = as.factor(Classification_name) ~ group+ WC2_BIO13+ WC2_BIO6+
                       group* WC2_BIO13+ WC2_BIO13+ WC2_BIO6 + group* WC2_BIO6+ group*WC2_BIO13* WC2_BIO6,
                      ntree=1000, data = Iberian_Month_1)
print(model4)
round(importance(model4), 2)

Europe_Quarter_1 <-Europe_Quarter[-c(192, 233:235, 335,336, 488:491, 531, 533:535),]
Europe_Quarter_1$WC2_BIO16 <- as.numeric(Europe_Quarter_1$WC2_BIO16)
Europe_Quarter_1 <- droplevels(Europe_Quarter_1)
str(Europe_Quarter_1)
set.seed(1234)
model5 <- randomForest(formula = as.factor(Classification_name) ~ group+ WC2_BIO16+ CHELSA_BIO11+
                         group* WC2_BIO16+ WC2_BIO16* CHELSA_BIO11 +group* CHELSA_BIO11+ group* WC2_BIO16* CHELSA_BIO11,
                       ntree=1000, data = Europe_Quarter_1)
print(model5)
round(importance(model5), 2)


Europe_Month_1 <-Europe_Month[-c(192, 233:235, 335,336, 488:491, 531, 533:535),]
Europe_Month_1$WC2_BIO13 <- as.numeric(Europe_Month_1$WC2_BIO13)
Europe_Month_1 <- droplevels(Europe_Month_1)
str(Europe_Quarter_1)
set.seed(1234)
model6 <- randomForest(formula = as.factor(Classification_name) ~ group+ WC2_BIO13+ WC2_BIO6+ 
                         group* WC2_BIO13+ WC2_BIO13* WC2_BIO6+ group*WC2_BIO6+ group* WC2_BIO13*WC2_BIO6,
                     ntree=1000,  data = Europe_Month_1)
print(model6)
round(importance(model6), 2)

pred <- predict(model6, Europe_Month_1, type = "class")
# Checking classification accuracy
table(pred, Europe_Month_1$Classification_name)  

importance(model6)        
varImpPlot(model6)        


Italy_Balkan_Quarter$WC2_BIO16 <- as.numeric(Italy_Balkan_Quarter$WC2_BIO16)


set.seed(1234)
Italy_Balkan_Quarter <- droplevels(Italy_Balkan_Quarter)
model7 <- randomForest(formula = as.factor(Classification_name) ~ group+ WC2_BIO16+ CHELSA_BIO11+
                         group* WC2_BIO16+ WC2_BIO16* CHELSA_BIO11+ group*CHELSA_BIO11+ group* WC2_BIO16* CHELSA_BIO11,
                       ntree=1000, data = Italy_Balkan_Quarter)
print(model7)
round(importance(model7), 2)


Italy_Balkan_Month$WC2_BIO13 <- as.numeric(Italy_Balkan_Month$WC2_BIO13)
Italy_Balkan_Month <- droplevels(Italy_Balkan_Month)

set.seed(1234)
model8 <- randomForest(formula = as.factor(Classification_name) ~ group+ WC2_BIO13+ WC2_BIO6+
                         group* WC2_BIO13+ WC2_BIO13+ WC2_BIO6 + group* WC2_BIO6+ group*WC2_BIO13* WC2_BIO6,
                       ntree=1000, data = Italy_Balkan_Month)
print(model8)
round(importance(model8), 2)

Europe_Quarter_1 <-Europe_Quarter[-c(192, 233:235, 335,336, 488:491, 531, 533:535),]
Europe_Quarter_1$WC2_BIO16 <- as.numeric(Europe_Quarter_1$WC2_BIO16)
Europe_Quarter_1 <- droplevels(Europe_Quarter_1)
str(Europe_Quarter_1)
set.seed(1234)
model5 <- randomForest(formula = as.factor(Classification_name) ~ group+ WC2_BIO16+ CHELSA_BIO11+
                         group* WC2_BIO16+ WC2_BIO16* CHELSA_BIO11 +group* CHELSA_BIO11+ group+ WC2_BIO16+ CHELSA_BIO11,
                       ntree=1000, data = Europe_Quarter_1)
print(model5)
round(importance(model5), 2)







iris.mds <- cmdscale(1 - iris.rf$proximity, eig=TRUE)
op <- par(pty="s")
pairs(cbind(iris[,1:4], iris.mds$points), cex=0.6, gap=0,
      col=c("red", "green", "blue")[as.numeric(iris$Species)],
      main="Iris Data: Predictors and MDS of Proximity Based on RandomForest")
par(op)
print(iris.mds$GOF)


set.seed(100)
Quarter.rf <- randomForest(Classification_name ~ ., data=Data_Quarter, importance=TRUE)

install.packages("caret")
library(caret)
library(e1071)
set.seed(1234)
# Run the model
rf_default <- train(Classification_name~.,
                    data = Data_Quarter,
                    method = "rf",
                    metric = "Accuracy",
                    trControl = trControl)
# Print the results
print(rf_default)


data(iris)
set.seed(71)
str(iris)
iris.rf <- randomForest(Species ~ ., data=iris, importance=TRUE,
                        proximity=TRUE)
print(iris.rf)
## Look at variable importance:
round(importance(iris.rf), 2)
## Do MDS on 1 - proximity:
iris.mds <- cmdscale(1 - iris.rf$proximity, eig=TRUE)
op <- par(pty="s")
pairs(cbind(iris[,1:4], iris.mds$points), cex=0.6, gap=0,
      col=c("red", "green", "blue")[as.numeric(iris$Species)],
      main="Iris Data: Predictors and MDS of Proximity Based on RandomForest")
par(op)
print(iris.mds$GOF)

## The `unsupervised' case:
set.seed(17)
iris.urf <- randomForest(iris[, -5])
MDSplot(iris.urf, iris$Species)

## stratified sampling: draw 20, 30, and 20 of the species to grow each tree.
(iris.rf2 <- randomForest(iris[1:4], iris$Species, 
                          sampsize=c(20, 30, 20)))

## Regression:
## data(airquality)
set.seed(131)
ozone.rf <- randomForest(Ozone ~ ., data=airquality, mtry=3,
                         importance=TRUE, na.action=na.omit)
print(ozone.rf)
## Show "importance" of variables: higher value mean more important:
round(importance(ozone.rf), 2)

## "x" can be a matrix instead of a data frame:
set.seed(17)
x <- matrix(runif(5e2), 100)
y <- gl(2, 50)
(myrf <- randomForest(x, y))
(predict(myrf, x))

## "complicated" formula:
(swiss.rf <- randomForest(sqrt(Fertility) ~ . - Catholic + I(Catholic < 50),
                          data=swiss))
(predict(swiss.rf, swiss))
## Test use of 32-level factor as a predictor:
set.seed(1)
x <- data.frame(x1=gl(53, 10), x2=runif(530), y=rnorm(530))
(rf1 <- randomForest(x[-3], x[[3]], ntree=10))

## Grow no more than 4 nodes per tree:
(treesize(randomForest(Species ~ ., data=iris, maxnodes=4, ntree=30)))

## test proximity in regression
iris.rrf <- randomForest(iris[-1], iris[[1]], ntree=101, proximity=TRUE, oob.prox=FALSE)
str(iris.rrf$proximity)
# }
