setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA")
GSLs_data <- read.table(file="Master_GSL.csv", header=T, sep=",")

GSLs_data<-GSLs_data[!(GSLs_data$Country=="AFG"),]
GSLs_data<-GSLs_data[!(GSLs_data$Country=="USA"),]
GSLs_data<-GSLs_data[!(GSLs_data$Country=="CAN"),]
GSLs_data<-GSLs_data[!(GSLs_data$Country=="CHN"),]
GSLs_data<-GSLs_data[!(GSLs_data$Country=="CPV"),]
GSLs_data<-GSLs_data[!(GSLs_data$Country=="UZB"),]
GSLs_data<-GSLs_data[!(GSLs_data$Country=="TJK"),]
GSLs_data<-GSLs_data[!(GSLs_data$Country=="IND"),]
GSLs_data<-GSLs_data[!(GSLs_data$Country=="JPN"),]
GSLs_data<-GSLs_data[!(GSLs_data$Country=="KAZ"),]
GSLs_data<-GSLs_data[!(GSLs_data$Country=="KGZ"),]
GSLs_data<-GSLs_data[!(GSLs_data$Country=="MAR"),]

GSLs_data<-GSLs_data[!(GSLs_data$Name == "Bijisk-4"),]
GSLs_data<-GSLs_data[!(GSLs_data$Name == "Kolyv-2"),]
GSLs_data<-GSLs_data[!(GSLs_data$Name == "Kolyv-3"),]
GSLs_data<-GSLs_data[!(GSLs_data$Name == "Kly-1"),]
GSLs_data<-GSLs_data[!(GSLs_data$Name == "Kly-4"),]
GSLs_data<-GSLs_data[!(GSLs_data$Name == "Kolyv-5"),]
GSLs_data<-GSLs_data[!(GSLs_data$Name == "Kolyv-6"),]
GSLs_data<-GSLs_data[!(GSLs_data$Name == "Koz-2"),]
GSLs_data<-GSLs_data[!(GSLs_data$Name == "K-oze-1"),]
GSLs_data<-GSLs_data[!(GSLs_data$Name == "K-oze-3"),]
GSLs_data<-GSLs_data[!(GSLs_data$Name == "Masl-1"),]
GSLs_data<-GSLs_data[!(GSLs_data$Name == "Noveg-3"),]
GSLs_data<-GSLs_data[!(GSLs_data$Name == "Noveg-2"),]
GSLs_data<-GSLs_data[!(GSLs_data$Name == "Noveg-1"),]
GSLs_data<-GSLs_data[!(GSLs_data$Name == "Lebja-1"),]
GSLs_data<-GSLs_data[!(GSLs_data$Name == "Nosov-1"),]
GSLs_data<-GSLs_data[!(GSLs_data$Name == "Panke-1"),]
GSLs_data<-GSLs_data[!(GSLs_data$Name == "Rakit-1"),]
GSLs_data<-GSLs_data[!(GSLs_data$Name == "Rakit-3"),]
GSLs_data<-GSLs_data[!(GSLs_data$Name == "Basta-1"),]
GSLs_data<-GSLs_data[!(GSLs_data$Name == "Basta-2"),]
GSLs_data<-GSLs_data[!(GSLs_data$Name == "Basta-3"),]
GSLs_data<-GSLs_data[!(GSLs_data$Name == "Chaba-2"),]
GSLs_data<-GSLs_data[!(GSLs_data$Name == "Sever-1"),]
GSLs_data<-GSLs_data[!(GSLs_data$Name == "Balan-1"),]
GSLs_data<-GSLs_data[!(GSLs_data$Name == "Valm"),]
GSLs_data<-GSLs_data[!(GSLs_data$Name == "Stepn-1"),]
GSLs_data<-GSLs_data[!(GSLs_data$Name == "Stepn-2"),]
GSLs_data<-GSLs_data[!(GSLs_data$Name == "Adam-1"),]
GSLs_data<-GSLs_data[!(GSLs_data$Name == "Karag-1"),]
GSLs_data<-GSLs_data[!(GSLs_data$Name == "Karag-2"),]
GSLs_data<-GSLs_data[!(GSLs_data$Name == "Kidr-1"),]
GSLs_data<-GSLs_data[!(GSLs_data$Name == "Per-1"),]
GSLs_data<-GSLs_data[!(GSLs_data$Name == "Pi-0"),]
GSLs_data<-GSLs_data[!(GSLs_data$Classification_name == "?"),]

#Grouping by the mountains:
GSLs_data$Mountain <- NA
GSLs_data$Mountain[which(GSLs_data$Country == "ESP")] <- "South"
GSLs_data$Mountain[which(GSLs_data$Country == "POR")] <- "South"
GSLs_data$Mountain[which(GSLs_data$Country == "ITA")] <- "South"
GSLs_data$Mountain[which(GSLs_data$Country == "GRC")] <- "South"
GSLs_data$Mountain[which(GSLs_data$Country == "BUL")] <- "South"
GSLs_data$Mountain[which(GSLs_data$Country == "CRO")] <- "South"
GSLs_data$Mountain[which(GSLs_data$Country == "SRB")] <- "South"
GSLs_data$Mountain[which(GSLs_data$Country == "ROU")] <- "South"
GSLs_data$Mountain[which(GSLs_data$Country == "SVK")] <- "South"
GSLs_data$Mountain[which(GSLs_data$Country == "LBN")] <- "South"
GSLs_data$Mountain[which(GSLs_data$Country == "GEO")] <- "South"
GSLs_data$Mountain[which(GSLs_data$Country == "ARM")] <- "South"
GSLs_data$Mountain[which(GSLs_data$Name == "Gr-5")] <- "South"
GSLs_data$Mountain[which(GSLs_data$Name == "Gr-1")] <- "South"

GSLs_data$Mountain[is.na(GSLs_data$Mountain)] <- "North"

GSLs_data_1<- GSLs_data[,-c(1,3,4,28:48)]
colnames(GSLs_data_1)

setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/Manuscript/Tables")

write.csv(GSLs_data_1, file = "GSLs_data.csv")

#Preparing the file of row data:
GSLs_data <- read.table(file="GSLs_data.csv", header=T, sep=",")

setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses")
GSLs_row_data <- read.table(file="GSLs profiles by weight.csv", header=T, sep=",")

colnames(GSLs_data)
CS_list<-GSLs_data[,c(2, 26)]
View(CS_list)
GSLs_row_data_1<-GSLs_row_data[,c(1:7,15:16,18,20:24,29:36,39,41:42,45:49)]
GSLs_row_data_2<-merge(GSLs_row_data_1, CS_list, by="CS")


setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/Manuscript/Tables")
write.csv(GSLs_row_data_2, file = "GSLs_row_data.csv")

setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny")
query <- read.table(file="query.csv", header=T, sep=",")
query_new<-merge(query, CS_list, by="CS")

write.csv(query_new, file = "query.csv")



  