install.packages("randomForest")
library(randomForest)
install.packages("ElemStatLearn")
library(ElemStatLearn)

#rpubs.com/datascientiest/270685

setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Enviroment")
Accessions_data <- read.table(file="Data_traits_short.csv", header=T, sep=",")
Enviromental_data <- read.table(file="shiny climatesd short.csv", header=T, sep=",")

colnames(Accessions_data)
colnames(Enviromental_data)

#Quarter:
#WorldClim BIO16
#CHELSA BIO11
#Month:
#WorldClim BIO13
#WorldClim BIO6

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

Accessions_data<-Accessions_data[!(Accessions_data$Name == "Bijisk-4"),]
Accessions_data<-Accessions_data[!(Accessions_data$Name == "Kolyv-2"),]
Accessions_data<-Accessions_data[!(Accessions_data$Name == "Kolyv-3"),]
Accessions_data<-Accessions_data[!(Accessions_data$Name == "Kly-1"),]
Accessions_data<-Accessions_data[!(Accessions_data$Name == "Kly-4"),]
Accessions_data<-Accessions_data[!(Accessions_data$Name == "Kolyv-5"),]
Accessions_data<-Accessions_data[!(Accessions_data$Name == "Kolyv-6"),]
Accessions_data<-Accessions_data[!(Accessions_data$Name == "Koz-2"),]
Accessions_data<-Accessions_data[!(Accessions_data$Name == "K-oze-1"),]
Accessions_data<-Accessions_data[!(Accessions_data$Name == "K-oze-3"),]
Accessions_data<-Accessions_data[!(Accessions_data$Name == "Masl-1"),]
Accessions_data<-Accessions_data[!(Accessions_data$Name == "Noveg-3"),]
Accessions_data<-Accessions_data[!(Accessions_data$Name == "Noveg-2"),]
Accessions_data<-Accessions_data[!(Accessions_data$Name == "Noveg-1"),]
Accessions_data<-Accessions_data[!(Accessions_data$Name == "Lebja-1"),]
Accessions_data<-Accessions_data[!(Accessions_data$Name == "Nosov-1"),]
Accessions_data<-Accessions_data[!(Accessions_data$Name == "Panke-1"),]
Accessions_data<-Accessions_data[!(Accessions_data$Name == "Rakit-1"),]
Accessions_data<-Accessions_data[!(Accessions_data$Name == "Rakit-3"),]
Accessions_data<-Accessions_data[!(Accessions_data$Name == "Basta-1"),]
Accessions_data<-Accessions_data[!(Accessions_data$Name == "Basta-2"),]
Accessions_data<-Accessions_data[!(Accessions_data$Name == "Basta-3"),]
Accessions_data<-Accessions_data[!(Accessions_data$Name == "Chaba-2"),]
Accessions_data<-Accessions_data[!(Accessions_data$Name == "Sever-1"),]
Accessions_data<-Accessions_data[!(Accessions_data$Name == "Balan-1"),]
Accessions_data<-Accessions_data[!(Accessions_data$Name == "Valm"),]
Accessions_data<-Accessions_data[!(Accessions_data$Name == "Stepn-1"),]
Accessions_data<-Accessions_data[!(Accessions_data$Name == "Stepn-2"),]
Accessions_data<-Accessions_data[!(Accessions_data$Name == "Adam-1"),]
Accessions_data<-Accessions_data[!(Accessions_data$Name == "Karag-1"),]
Accessions_data<-Accessions_data[!(Accessions_data$Name == "Karag-2"),]
Accessions_data<-Accessions_data[!(Accessions_data$Name == "Kidr-1"),]
Accessions_data<-Accessions_data[!(Accessions_data$Name == "Per-1"),]
Accessions_data<-Accessions_data[!(Accessions_data$Name == "Pi-0"),]


Accessions_data<-Accessions_data[!(Accessions_data$Classification_name == "?"),]

Accessions_data[is.na(Accessions_data)] <- 0

colnames(Accessions_data)

Enviromental_w_g <-Enviromental_data[,c(1,2,80,81,88,89,198)]
Accessions_1 <- Accessions_data[,c(1,20,28)]

Data_Enviromental_w_g_1 <- merge(Accessions_1, Enviromental_w_g, by="CS")
Data_Enviromental_w_g <-Data_Enviromental_w_g_1[,-c(1)]



Data_Enviromental_w_g$WC2_BIO13<- as.numeric(Data_Enviromental_w_g$WC2_BIO13)
Data_Enviromental_w_g$WC2_BIO14<- as.numeric(Data_Enviromental_w_g$WC2_BIO14)
Data_Enviromental_w_g <- na.omit(Data_Enviromental_w_g) 




#Grouping by the mountains:
Data_Enviromental_w_g$Mountain <- NA
Data_Enviromental_w_g$Mountain[which(Data_Enviromental_w_g$Country == "ESP")] <- "South"
Data_Enviromental_w_g$Mountain[which(Data_Enviromental_w_g$Country == "POR")] <- "South"
Data_Enviromental_w_g$Mountain[which(Data_Enviromental_w_g$Country == "ITA")] <- "South"
Data_Enviromental_w_g$Mountain[which(Data_Enviromental_w_g$Country == "GRC")] <- "South"
Data_Enviromental_w_g$Mountain[which(Data_Enviromental_w_g$Country == "BUL")] <- "South"
Data_Enviromental_w_g$Mountain[which(Data_Enviromental_w_g$Country == "CRO")] <- "South"
Data_Enviromental_w_g$Mountain[which(Data_Enviromental_w_g$Country == "SRB")] <- "South"
Data_Enviromental_w_g$Mountain[which(Data_Enviromental_w_g$Country == "ROU")] <- "South"
Data_Enviromental_w_g$Mountain[which(Data_Enviromental_w_g$Country == "SVK")] <- "South"
Data_Enviromental_w_g$Mountain[which(Data_Enviromental_w_g$Country == "LBN")] <- "South"
Data_Enviromental_w_g$Mountain[which(Data_Enviromental_w_g$Country == "GEO")] <- "South"
Data_Enviromental_w_g$Mountain[which(Data_Enviromental_w_g$Country == "ARM")] <- "South"
Data_Enviromental_w_g$Mountain[which(Data_Enviromental_w_g$Name == "Gr-5")] <- "South"
Data_Enviromental_w_g$Mountain[which(Data_Enviromental_w_g$Name == "Gr-1")] <- "South"

Data_Enviromental_w_g$Mountain[is.na(Data_Enviromental_w_g$Mountain)] <- "North"

#Separating the Mountain:

Data_Enviromental_w_g$Mountain <- as.factor(Data_Enviromental_w_g$Mountain)
Data_Enviromental_w_g_Mountain <- split(Data_Enviromental_w_g, Data_Enviromental_w_g$Mountain)
list2env(Data_Enviromental_w_g_Mountain, envir=.GlobalEnv)
rm(Data_Enviromental_w_g_Mountain)


Data_Enviromental_w_gNorth <- droplevels.data.frame(North)
Data_Enviromental_w_gSouth <- droplevels.data.frame(South)



#Subseting the data by chemotypes:

#North


str(Data_Enviromental_w_gNorth_1)
Data_Enviromental_w_gNorth<-Data_Enviromental_w_gNorth[!(Data_Enviromental_w_gNorth$Classification_name=="3OHP"),]
Data_Enviromental_w_gNorth<-Data_Enviromental_w_gNorth[!(Data_Enviromental_w_gNorth$Classification_name=="4MSO"),]
Data_Enviromental_w_gNorth<-Data_Enviromental_w_gNorth[!(Data_Enviromental_w_gNorth$Classification_name=="Butenyl"),]
Data_Enviromental_w_gNorth<-Data_Enviromental_w_gNorth[!(Data_Enviromental_w_gNorth$Classification_name=="3MSO"),]
Data_Enviromental_w_gNorth<-Data_Enviromental_w_gNorth[!(Data_Enviromental_w_gNorth$Classification_name=="4OHB"),]
Data_Enviromental_w_gNorth_1<-Data_Enviromental_w_gNorth[,-c(2,9)]

Data_Enviromental_w_gNorth_1 <- droplevels.data.frame(Data_Enviromental_w_gNorth_1)

#South


str(Data_Enviromental_w_gSouth)
Data_Enviromental_w_gSouth<-Data_Enviromental_w_gSouth[!(Data_Enviromental_w_gSouth$Classification_name=="3OHP"),]
Data_Enviromental_w_gSouth<-Data_Enviromental_w_gSouth[!(Data_Enviromental_w_gSouth$Classification_name=="4MSO"),]
Data_Enviromental_w_gSouth<-Data_Enviromental_w_gSouth[!(Data_Enviromental_w_gSouth$Classification_name=="Butenyl"),]
Data_Enviromental_w_gSouth<-Data_Enviromental_w_gSouth[!(Data_Enviromental_w_gSouth$Classification_name=="3MSO"),]
Data_Enviromental_w_gSouth<-Data_Enviromental_w_gSouth[!(Data_Enviromental_w_gSouth$Classification_name=="4OHB"),]
Data_Enviromental_w_gSouth_1<-Data_Enviromental_w_gSouth[,-c(2,9)]

Data_Enviromental_w_gSouth_1 <- droplevels.data.frame(Data_Enviromental_w_gSouth_1)


#North:

train <- sample(1:nrow(Data_Enviromental_w_gNorth_1), nrow(Data_Enviromental_w_gNorth_1)*2/3)
spam_train <- Data_Enviromental_w_gNorth_1[train,]
spam_test <- Data_Enviromental_w_gNorth_1[-train,]
summary(spam_train)

spam.bag <- randomForest(Classification_name~., data=spam_train, mtry=(dim(Data_Enviromental_w_gNorth_1)[2]-1))
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
rf.North=randomForest(Classification_name ~., data=spam_train,  importance=TRUE, mtry=mtry_x, ntree=ntree_x)

#predict on test set:
yhat <- predict(rf.North, spam_test, type = 'class')
y <- spam_test$Classification_name
table(y,yhat) 
mean(y != yhat)
#Find the 10 most important variables for the random Forest model.:
varImpPlot(rf.North, sort=TRUE, n.var=6) 

#the importance of every variable in classifying the data:
best <- as.data.frame(round(importance(rf.North), 2)) # get importance values
top <- best[order(-best$MeanDecreaseGini), , drop = FALSE] # sort highest first
top10 <- top[1:6, , drop=FALSE]
top10


#South:
train <- sample(1:nrow(Data_Enviromental_w_gSouth_1), nrow(Data_Enviromental_w_gSouth_1)*2/3)
spam_train <- Data_Enviromental_w_gSouth_1[train,]
spam_test <- Data_Enviromental_w_gSouth_1[-train,]
summary(spam_train)

spam.bag <- randomForest(Classification_name~., data=spam_train, mtry=(dim(Data_Enviromental_w_gSouth_1)[2]-1))
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
rf.South=randomForest(Classification_name ~., data=spam_train,  importance=TRUE, mtry=mtry_x, ntree=ntree_x)

#predict on test set:
yhat <- predict(rf.South, spam_test, type = 'class')
y <- spam_test$Classification_name
table(y,yhat) 
mean(y != yhat)
#Find the 10 most important variables for the random Forest model.:
varImpPlot(rf.South, sort=TRUE, n.var=6) 

#the importance of every variable in classifying the data:
best <- as.data.frame(round(importance(rf.South), 2)) # get importance values
top <- best[order(-best$MeanDecreaseGini), , drop = FALSE] # sort highest first
top10 <- top[1:6, , drop=FALSE]
top10

#Without group:
colnames(Data_Enviromental_w_gNorth_1)
Data_Enviromental_w_gNorth_2 <- Data_Enviromental_w_gNorth_1[,-c(2)]
Data_Enviromental_w_gSouth_2 <- Data_Enviromental_w_gSouth_1[,-c(2)]

Data_Enviromental_w_gNorth_2 <- droplevels.data.frame(Data_Enviromental_w_gNorth_2)
Data_Enviromental_w_gSouth_2 <- droplevels.data.frame(Data_Enviromental_w_gSouth_2)

#North:

train <- sample(1:nrow(Data_Enviromental_w_gNorth_2), nrow(Data_Enviromental_w_gNorth_2)*2/3)
spam_train <- Data_Enviromental_w_gNorth_2[train,]
spam_test <- Data_Enviromental_w_gNorth_2[-train,]
summary(spam_train)

spam.bag <- randomForest(Classification_name~., data=spam_train, mtry=(dim(Data_Enviromental_w_gNorth_2)[2]-1))
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
rf.North=randomForest(Classification_name ~., data=spam_train,  importance=TRUE, mtry=mtry_x, ntree=ntree_x)

#predict on test set:
yhat <- predict(rf.North, spam_test, type = 'class')
y <- spam_test$Classification_name
table(y,yhat) 
mean(y != yhat)
#Find the 10 most important variables for the random Forest model.:
varImpPlot(rf.North, sort=TRUE, n.var=5) 

#the importance of every variable in classifying the data:
best <- as.data.frame(round(importance(rf.North), 2)) # get importance values
top <- best[order(-best$MeanDecreaseGini), , drop = FALSE] # sort highest first
top10 <- top[1:5, , drop=FALSE]
top10


#South:
train <- sample(1:nrow(Data_Enviromental_w_gSouth_2), nrow(Data_Enviromental_w_gSouth_2)*2/3)
spam_train <- Data_Enviromental_w_gSouth_2[train,]
spam_test <- Data_Enviromental_w_gSouth_2[-train,]
summary(spam_train)

spam.bag <- randomForest(Classification_name~., data=spam_train, mtry=(dim(Data_Enviromental_w_gSouth_2)[2]-1))
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
rf.South=randomForest(Classification_name ~., data=spam_train,  importance=TRUE, mtry=mtry_x, ntree=ntree_x)

#predict on test set:
yhat <- predict(rf.South, spam_test, type = 'class')
y <- spam_test$Classification_name
table(y,yhat) 
mean(y != yhat)
#Find the 10 most important variables for the random Forest model.:
varImpPlot(rf.South, sort=TRUE, n.var=5) 

#the importance of every variable in classifying the data:
best <- as.data.frame(round(importance(rf.South), 2)) # get importance values
top <- best[order(-best$MeanDecreaseGini), , drop = FALSE] # sort highest first
top10 <- top[1:5, , drop=FALSE]
top10

