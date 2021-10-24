#Install the packages:

install.packages('qqman')
library("qqman")
library(tidyverse)

#Load all the csv files:

setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/GWAS/PC12/NEW/PC1 5%")

All_PC1_CHR13 <- read.table(file="PC1 CHR1_3a.csv", header=T, sep=",")
All_PC1_CHR35 <- read.table(file="PC1 CHR3_5.csv", header=T, sep=",")

#Combine all the csv files:

All_PC1_CHR <- rbind(All_PC1_CHR13, All_PC1_CHR35)
colnames(All_PC1_CHR)

All_PC1_CHR$CHR <- as.numeric(as.numeric(All_PC1_CHR$CHR))

#Manhattan plot (it takes a while):

manhattan(All_PC1_CHR, main = "Manhattan Plot, PC1")
manhattan(All_PC1_CHR, cex=0.8, col=(c("grey", "skyblue")),  plot.margin = unit(c(1,4,1,1),"cm"))

