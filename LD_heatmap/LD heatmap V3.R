library(tidyverse)
library(VariantAnnotation)
#library(vcfR)
library(genetics)
library(LDheatmap)

vcf_i <- read.vcfR("~/Downloads/Chr4_1250000_1600000.vcf", verbose = FALSE )
IndelDataGT <- extract.gt(vcf_i, element="GT")
IndelDataGT_2 <- gsub("\\|", "/", IndelDataGT)

#AOP:
setwd("C:/Users/Ella Katz/Desktop/LD heatmap/Celine Practice/LD heatmap practice/From Dexter")
IndelDataGT_2 <-read.table(file="Chr4_1250000_1600000.txt", header=T, sep="\t")
AOP_PC1<-read.table(file="AOP_area_PC1.csv", header=T, sep=",")
AOP_PC1$BP <- sub("^", "4_", AOP_PC1$BP )
AOP_PC1_P <- subset(AOP_PC1, P <= 0.01)
AOP_PC1_P_1 <- AOP_PC1_P[,c(2,3)]
rownames(AOP_PC1_P_1) <- AOP_PC1_P[,2]

Indel_AOP <- merge(IndelDataGT_2, AOP_PC1_P_1, by = 0)   
Indel_AOP_1 <-Indel_AOP[,c(2:1136)]
rownames(Indel_AOP_1) <- Indel_AOP[,1]


df2 <- data.frame(t(Indel_AOP_1[]))
df3 <-  df2[ , colSums(is.na(df2))<nrow(df2)]
class(df3[,1])

df3[] <- lapply(df3, as.genotype)
class(df3[,1])

locations_1<- names(df3)
locations_2<- gsub("X4_", "", locations_1)
locations_2<-as.numeric(locations_2)

my_palette <- colorRampPalette(c("red","yellow",  "springgreen4",  "steelblue3" , "purple", "black"))(n = 299)

View(locations_2)
MyHeatmap <- LDheatmap(df3, genetic.distances = locations_2,
                       color = grey.colors(20))
MyHeatmap_1 <- LDheatmap(df3, genetic.distances = locations_2,
                       color = my_palette)


MyHeatmap_2 <- LDheatmap(df3, genetic.distances = locations_2,
                       color = "blueToRed",
                       title="AOP", SNP.name=c("X4_1351577", "X4_1351577"))
colnames(df3)
flip=TRUE, 



#PC2:
AOP_PC2<-read.table(file="AOP_area_PC2.csv", header=T, sep=",")
AOP_PC2$BP <- sub("^", "4_", AOP_PC2$BP )
AOP_PC2_P <- subset(AOP_PC2, P <= 0.01)
AOP_PC2_P_1 <- AOP_PC2_P[,c(2,3)]
rownames(AOP_PC2_P_1) <- AOP_PC2_P[,2]

Indel_AOP_a <- merge(IndelDataGT_2, AOP_PC2_P_1, by = 0)   
Indel_AOP_2 <-Indel_AOP_a[,c(2:1136)]
rownames(Indel_AOP_2) <- Indel_AOP_a[,1]


df4 <- data.frame(t(Indel_AOP_2[]))
df5 <-  df4[ , colSums(is.na(df4))<nrow(df4)]
class(df5[,1])

df5[] <- lapply(df5, as.genotype)
class(df5[,1])

locations_3<- names(df5)
locations_4<- gsub("X4_", "", locations_3)
locations_4<-as.numeric(locations_4)

MyHeatmap <- LDheatmap(df5, genetic.distances = locations_4,
                       color = grey.colors(20))
MyHeatmap_1 <- LDheatmap(df5, genetic.distances = locations_4,
                         color = my_palette)
#PC1 and PC2
AOP_all <- rbind(AOP_PC2_P, AOP_PC1_P)
AOP_all_1<- AOP_all[,c(1,2)]
AOP_all_1<-unique(AOP_all_1)
rownames(AOP_all_1) <- AOP_all_1[,2]

Indel_AOP_all <- merge(IndelDataGT_2, AOP_all_1, by = 0)   
Indel_AOP_all_1 <-Indel_AOP_all[,c(2:1136)]
rownames(Indel_AOP_all_1) <- Indel_AOP_all[,1]


df6 <- data.frame(t(Indel_AOP_all_1[]))
df7 <-  df6[ , colSums(is.na(df6))<nrow(df6)]
class(df7[,1])

df7[] <- lapply(df7, as.genotype)
class(df7[,1])

locations_5<- names(df7)
locations_6<- gsub("X4_", "", locations_5)
locations_6<-as.numeric(locations_6)

MyHeatmap_2 <- LDheatmap(df7, genetic.distances = locations_6,
                         color = my_palette)

#MAM:
setwd("C:/Users/Ella Katz/Desktop/LD heatmap/Celine Practice/LD heatmap practice/From Dexter")
IndelDataGT_2 <-read.table(file="Chr5_7500000_8200000.txt", header=T, sep="\t")
MAM_PC1<-read.table(file="MAM_area_PC1.csv", header=T, sep=",")
MAM_PC1$BP <- sub("^", "5_", MAM_PC1$BP )
MAM_PC1_P <- subset(MAM_PC1, P <= 0.01)
MAM_PC1_P_1 <- MAM_PC1_P[,c(2,3)]
rownames(MAM_PC1_P_1) <- MAM_PC1_P[,2]

Indel_MAM <- merge(IndelDataGT_2, MAM_PC1_P_1, by = 0)   
Indel_MAM_1 <-Indel_MAM[,c(2:1136)]
rownames(Indel_MAM_1) <- Indel_MAM[,1]


df2 <- data.frame(t(Indel_MAM_1[]))
df3 <-  df2[ , colSums(is.na(df2))<nrow(df2)]
class(df3[,1])

df3[] <- lapply(df3, as.genotype)
class(df3[,1])

locations_1<- names(df3)
locations_2<- gsub("X5_", "", locations_1)
locations_2<-as.numeric(locations_2)

my_palette <- colorRampPalette(c("red","yellow",  "springgreen4",  "steelblue3" , "purple", "black"))(n = 299)

View(locations_2)
MyHeatmap <- LDheatmap(df3, genetic.distances = locations_2,
                       color = grey.colors(20))
MyHeatmap_1 <- LDheatmap(df3, genetic.distances = locations_2,
                         color = my_palette)

#PC2:
MAM_PC2<-read.table(file="MAM_area_PC2.csv", header=T, sep=",")
MAM_PC2$BP <- sub("^", "5_", MAM_PC2$BP )
MAM_PC2_P <- subset(MAM_PC2, P <= 0.01)
MAM_PC2_P_1 <- MAM_PC2_P[,c(2,3)]
rownames(MAM_PC2_P_1) <- MAM_PC2_P[,2]

Indel_MAM_a <- merge(IndelDataGT_2, MAM_PC2_P_1, by = 0)   
Indel_MAM_2 <-Indel_MAM_a[,c(2:1136)]
rownames(Indel_MAM_2) <- Indel_MAM_a[,1]


df4 <- data.frame(t(Indel_MAM_2[]))
df5 <-  df4[ , colSums(is.na(df4))<nrow(df4)]
class(df5[,1])

df5[] <- lapply(df5, as.genotype)
class(df5[,1])

locations_3<- names(df5)
locations_4<- gsub("X5_", "", locations_3)
locations_4<-as.numeric(locations_4)

MyHeatmap <- LDheatmap(df5, genetic.distances = locations_4,
                       color = grey.colors(20))
MyHeatmap_1 <- LDheatmap(df5, genetic.distances = locations_4,
                         color = my_palette)
#PC1 and PC2
MAM_all <- rbind(MAM_PC2_P, MAM_PC1_P)
MAM_all_1<- MAM_all[,c(1,2)]
MAM_all_1<-unique(MAM_all_1)
rownames(MAM_all_1) <- MAM_all_1[,2]

Indel_MAM_all <- merge(IndelDataGT_2, MAM_all_1, by = 0)   
Indel_MAM_all_1 <-Indel_MAM_all[,c(2:1136)]
rownames(Indel_MAM_all_1) <- Indel_MAM_all[,1]


df6 <- data.frame(t(Indel_MAM_all_1[]))
df7 <-  df6[ , colSums(is.na(df6))<nrow(df6)]
class(df7[,1])

df7[] <- lapply(df7, as.genotype)
class(df7[,1])

locations_5<- names(df7)
locations_6<- gsub("X5_", "", locations_5)
locations_6<-as.numeric(locations_6)

MyHeatmap_2 <- LDheatmap(df7, genetic.distances = locations_6,
                         color = my_palette)


#PC1-10:
#MAM:
setwd("C:/Users/Ella Katz/Desktop/LD heatmap/Celine Practice/LD heatmap practice/From Dexter")
IndelDataGT_2 <-read.table(file="Chr5_7500000_8200000.txt", header=T, sep="\t")
MAM_PC1a<-read.table(file="MAM_area_PC1.csv", header=T, sep=",")

setwd("C:/Users/Ella Katz/Desktop/LD heatmap/Celine Practice/LD heatmap practice/From Dexter/Dexters_PCs")
MAM_PC1<-read.table(file="PC1.csv", header=T, sep=",")
MAM_PC1_P <- subset(MAM_PC1, P <= 0.01)
MAM_PC1_P <- subset(MAM_PC1, CHR = 5)
MAM_PC1_P_1 <- MAM_PC1_P[,c(2,3)]
rownames(MAM_PC1_P_1) <- MAM_PC1_P[,2]

MAM_PC2<-read.table(file="PC2.csv", header=T, sep=",")
MAM_PC2_P <- subset(MAM_PC2, P <= 0.01)
MAM_PC2_P <- subset(MAM_PC2, CHR = 5)
MAM_PC2_P_1 <- MAM_PC2_P[,c(2,3)]
rownames(MAM_PC2_P_1) <- MAM_PC2_P[,2]

MAM_PC3<-read.table(file="PC3.csv", header=T, sep=",")
MAM_PC3_P <- subset(MAM_PC3, P <= 0.01)
MAM_PC3_P <- subset(MAM_PC3, CHR = 5)
MAM_PC3_P_1 <- MAM_PC3_P[,c(2,3)]
rownames(MAM_PC3_P_1) <- MAM_PC3_P[,2]

MAM_PC4<-read.table(file="PC4.csv", header=T, sep=",")
MAM_PC4_P <- subset(MAM_PC4, P <= 0.01)
MAM_PC4_P <- subset(MAM_PC4, CHR = 5)
MAM_PC4_P_1 <- MAM_PC4_P[,c(2,3)]
rownames(MAM_PC4_P_1) <- MAM_PC4_P[,2]

MAM_PC5<-read.table(file="PC5.csv", header=T, sep=",")
MAM_PC5_P <- subset(MAM_PC5, P <= 0.01)
MAM_PC5_P <- subset(MAM_PC5, CHR = 5)
MAM_PC5_P_1 <- MAM_PC5_P[,c(2,3)]
rownames(MAM_PC5_P_1) <- MAM_PC5_P[,2]

MAM_PC6<-read.table(file="PC6.csv", header=T, sep=",")
MAM_PC6_P <- subset(MAM_PC6, P <= 0.01)
MAM_PC6_P <- subset(MAM_PC6, CHR = 5)
MAM_PC6_P_1 <- MAM_PC6_P[,c(2,3)]
rownames(MAM_PC6_P_1) <- MAM_PC6_P[,2]

MAM_PC7<-read.table(file="PC7.csv", header=T, sep=",")
MAM_PC7_P <- subset(MAM_PC7, P <= 0.01)
MAM_PC7_P <- subset(MAM_PC7, CHR = 5)
MAM_PC7_P_1 <- MAM_PC7_P[,c(2,3)]
rownames(MAM_PC7_P_1) <- MAM_PC7_P[,2]

MAM_PC8<-read.table(file="PC8.csv", header=T, sep=",")
MAM_PC8_P <- subset(MAM_PC8, P <= 0.01)
MAM_PC8_P <- subset(MAM_PC8, CHR = 5)
MAM_PC8_P_1 <- MAM_PC8_P[,c(2,3)]
rownames(MAM_PC8_P_1) <- MAM_PC8_P[,2]

MAM_PC9<-read.table(file="PC9.csv", header=T, sep=",")
MAM_PC9_P <- subset(MAM_PC9, P <= 0.01)
MAM_PC9_P <- subset(MAM_PC9, CHR = 5)
MAM_PC9_P_1 <- MAM_PC9_P[,c(2,3)]
rownames(MAM_PC9_P_1) <- MAM_PC9_P[,2]

MAM_PC10<-read.table(file="PC10.csv", header=T, sep=",")
MAM_PC10_P <- subset(MAM_PC10, P <= 0.01)
MAM_PC10_P <- subset(MAM_PC10, CHR = 5)
MAM_PC10_P_1 <- MAM_PC10_P[,c(2,3)]
rownames(MAM_PC10_P_1) <- MAM_PC10_P[,2]

MAM_all <- rbind(MAM_PC2_P, MAM_PC1_P, MAM_PC3_P, MAM_PC4_P, MAM_PC5_P, MAM_PC6_P,
                 MAM_PC7_P, MAM_PC8_P, MAM_PC9_P, MAM_PC10_P)
MAM_all_1<- MAM_all[,c(1,2)]
MAM_all_1<-unique(MAM_all_1)
rownames(MAM_all_1) <- MAM_all_1[,2]

Indel_MAM_all <- merge(IndelDataGT_2, MAM_all_1, by = 0)   
Indel_MAM_all_1 <-Indel_MAM_all[,c(2:1136)]
rownames(Indel_MAM_all_1) <- Indel_MAM_all[,1]


df8 <- data.frame(t(Indel_MAM_all_1[]))
df9 <-  df8[ , colSums(is.na(df8))<nrow(df8)]
class(df9[,1])

df9[] <- lapply(df9, as.genotype)
class(df9[,1])

locations_7<- names(df9)
locations_8<- gsub("X5_", "", locations_7)
locations_8<-as.numeric(locations_8)


my_palette <- colorRampPalette(c("red","yellow",  "springgreen4",  "steelblue3" , "purple", "black"))(n = 299)
MyHeatmap_2 <- LDheatmap(df9, genetic.distances = locations_8,
                         color = my_palette)

#AOP:
setwd("C:/Users/Ella Katz/Desktop/LD heatmap/Celine Practice/LD heatmap practice/From Dexter")
IndelDataGT_2 <-read.table(file="Chr4_1250000_1600000.txt", header=T, sep="\t")
AOP_PC1<-read.table(file="AOP_area_PC1.csv", header=T, sep=",")
AOP_PC1$BP <- sub("^", "4_", AOP_PC1$BP )
AOP_PC1_P <- subset(AOP_PC1, P <= 0.01)
AOP_PC1_P_1 <- AOP_PC1_P[,c(2,3)]
rownames(AOP_PC1_P_1) <- AOP_PC1_P[,2]


AOP_PC2<-read.table(file="AOP_area_PC2.csv", header=T, sep=",")
AOP_PC2$BP <- sub("^", "4_", AOP_PC2$BP )
AOP_PC2_P <- subset(AOP_PC2, P <= 0.01)
AOP_PC2_P_1 <- AOP_PC2_P[,c(2,3)]
rownames(AOP_PC2_P_1) <- AOP_PC2_P[,2]

AOP_PC3_P <- subset(MAM_PC3, CHR = 4)
AOP_PC3_P_1 <- AOP_PC3_P[,c(2,3)]
rownames(AOP_PC3_P_1) <- AOP_PC3_P[,2]

AOP_PC4_P <- subset(MAM_PC4, CHR = 4)
AOP_PC4_P_1 <- AOP_PC4_P[,c(2,3)]
rownames(AOP_PC4_P_1) <- AOP_PC4_P[,2]

AOP_PC5_P <- subset(MAM_PC5, CHR = 4)
AOP_PC5_P_1 <- AOP_PC5_P[,c(2,3)]
rownames(AOP_PC5_P_1) <- AOP_PC5_P[,2]

AOP_PC6_P <- subset(MAM_PC6, CHR = 4)
AOP_PC6_P_1 <- AOP_PC6_P[,c(2,3)]
rownames(AOP_PC6_P_1) <- AOP_PC6_P[,2]

AOP_PC7_P <- subset(MAM_PC7, CHR = 4)
AOP_PC7_P_1 <- AOP_PC7_P[,c(2,3)]
rownames(AOP_PC7_P_1) <- AOP_PC7_P[,2]

AOP_PC8_P <- subset(MAM_PC8, CHR = 4)
AOP_PC8_P_1 <- AOP_PC8_P[,c(2,3)]
rownames(AOP_PC8_P_1) <- AOP_PC8_P[,2]

AOP_PC9_P <- subset(MAM_PC9, CHR = 4)
AOP_PC9_P_1 <- AOP_PC9_P[,c(2,3)]
rownames(AOP_PC9_P_1) <- AOP_PC9_P[,2]

AOP_PC10_P <- subset(MAM_PC10, CHR = 4)
AOP_PC10_P_1 <- AOP_PC10_P[,c(2,3)]
rownames(AOP_PC10_P_1) <- AOP_PC10_P[,2]

AOP_all <- rbind(AOP_PC2_P, AOP_PC1_P, AOP_PC3_P, AOP_PC4_P, AOP_PC5_P, AOP_PC6_P,
                 AOP_PC7_P, AOP_PC8_P, AOP_PC9_P, AOP_PC10_P)
AOP_all_1<- AOP_all[,c(1,2)]
AOP_all_1<-unique(AOP_all_1)
rownames(AOP_all_1) <- AOP_all_1[,2]

Indel_AOP_a <- merge(IndelDataGT_2, AOP_all_1, by = 0)   
Indel_AOP_2 <-Indel_AOP_a[,c(2:1136)]
rownames(Indel_AOP_2) <- Indel_AOP_a[,1]


df10 <- data.frame(t(Indel_AOP_2[]))
df11 <-  df10[ , colSums(is.na(df10))<nrow(df10)]
class(df11[,1])

df11[] <- lapply(df11, as.genotype)
class(df11[,1])

locations_9<- names(df11)
locations_10<- gsub("X4_", "", locations_9)
locations_10<-as.numeric(locations_10)

MyHeatmap_2 <- LDheatmap(df11, genetic.distances = locations_10,
                         color = my_palette)

#Plots:
setwd("C:/Users/Ella Katz/Desktop/LD heatmap/Celine Practice/LD heatmap practice/From Dexter")
MAM_PC1<-read.table(file="MAM_area_PC1.csv", header=T, sep=",")
MAM_PC1$BP <- sub("^", "5.", MAM_PC1$BP )
MAM_PC1_P <- subset(MAM_PC1, P <= 0.01)
MAM_PC1_P_1 <- MAM_PC1_P[,c(2,3)]
MAM_PC1_P_1$PC<-"PC1"
MAM_PC1_P_1$BP<-as.numeric(MAM_PC1_P_1$BP)


MAM_PC2<-read.table(file="MAM_area_PC2.csv", header=T, sep=",")
MAM_PC2$BP <- sub("^", "5.", MAM_PC2$BP )
MAM_PC2_P <- subset(MAM_PC2, P <= 0.01)
MAM_PC2_P_1 <- MAM_PC2_P[,c(2,3)]
MAM_PC2_P_1$PC<-"PC2"
MAM_PC2_P_1$BP<-as.numeric(MAM_PC2_P_1$BP)


str(MAM_PC1_P_1)
MAM_PC12_P_1<-rbind(MAM_PC1_P_1, MAM_PC2_P_1)
MAM_PC12_P_1a <- mutate(MAM_PC12_P_1, P_log= (-log10(P)))


ggplot(MAM_PC12_P_1a, aes(x=BP, y=P_log, color=PC)) + 
    geom_point()+theme_classic()+
  scale_color_manual(values = c("blue", "green"))

setwd("C:/Users/Ella Katz/Desktop/LD heatmap/Celine Practice/LD heatmap practice/From Dexter/Dexters_PCs")
MAM_PC1<-read.table(file="PC1.csv", header=T, sep=",")
MAM_PC1$BP<-gsub("_", ".", MAM_PC1$BP)
MAM_PC1_1 <- MAM_PC1[,c(2,3)]
MAM_PC1_1$PC<-"PC1"
MAM_PC1_1$BP<-as.numeric(MAM_PC1_1$BP)

MAM_PC2<-read.table(file="PC2.csv", header=T, sep=",")
MAM_PC2$BP<-gsub("_", ".", MAM_PC2$BP)
MAM_PC2_1 <- MAM_PC2[,c(2,3)]
MAM_PC2_1$PC<-"PC2"
MAM_PC2_1$BP<-as.numeric(MAM_PC2_1$BP)


MAM_PC3<-read.table(file="PC3.csv", header=T, sep=",")
MAM_PC3$BP<-gsub("_", ".", MAM_PC3$BP)
MAM_PC3_1 <- MAM_PC3[,c(2,3)]
MAM_PC3_1$PC<-"PC3"
MAM_PC3_1$BP<-as.numeric(MAM_PC3_1$BP)

MAM_PC4<-read.table(file="PC4.csv", header=T, sep=",")
MAM_PC4$BP<-gsub("_", ".", MAM_PC4$BP)
MAM_PC4_1 <- MAM_PC4[,c(2,3)]
MAM_PC4_1$PC<-"PC4"
MAM_PC4_1$BP<-as.numeric(MAM_PC4_1$BP)


MAM_PC13_P_1<-rbind(MAM_PC1_1, MAM_PC2_1, MAM_PC3_1, MAM_PC4_1)
MAM_PC13_P_1a <- mutate(MAM_PC13_P_1, P_log= (-log10(P)))


ggplot(MAM_PC13_P_1a, aes(x=BP, y=P_log, color=PC)) + 
  geom_point(alpha=0.7)+theme_classic()+
  scale_color_manual(values = c("blue", "green", "red", "yellow"))+
  xlim(5.750008, 5.819938)+
  geom_hline(yintercept = 5, color="blue")+
  geom_hline(yintercept = 7.5, color="red")+
  ggtitle("MAM area, PC1-4")+
  geom_vline(xintercept = 5.7703041)+
  geom_vline(xintercept = 5.7721866)
