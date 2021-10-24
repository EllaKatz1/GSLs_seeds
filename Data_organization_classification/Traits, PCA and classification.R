install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")
install.packages('corrplot')
library("corrplot")
library(gplots)
library(tidyverse)
library(lme4)
library(lmerTest)
library(wesanderson)
library(reshape)
install.packages('ggmap')
library(ggmap)
install.packages('CRAN')
library("rjson")
library(maps)
install.packages("RJSONIO")
library(RJSONIO)
install.packages("googleway") 
library("googleway")
install.packages("emmeans")
library(emmeans)
library(multcomp)

#citation("ggmap")

#All the amounts are umol/mg

#Details:
setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses")
details <- read.table(file="GSLs profiles by weight.csv", header=T, sep=",")

setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/Stat/emmeans")
data <- read.table(file="Emmeans_data.csv", header=T, sep=",")


#na.omit(data)
data[is.na(data)] <- 0

colnames(details)
details_1 <- details[,c(6:10)] 

Total_data <- merge(details_1, data, by="CS")
Total_data <- droplevels.data.frame(Total_data)
Total_data_1 <- unique(Total_data)
Total_data_2 <-Total_data_1[, -c(6,7,31:33)] 
write.csv(Total_data_2, file = "Total_data.csv")

#Calculating the traits based on the emmeans:


#Starting to classifie the accessions by chemotypes:
colnames(data)
data[is.na(data)] <- 0

Data_02<-merge(details_1, data, by="CS")
Data_02<-droplevels.data.frame(Data_02)
Data_03<- unique(Data_02)

Data_01 <- mutate(Data_03, C3= (X3OHP + X3MSO + Allyl + X3MT),
                 C4= (X4OHB + OHBut + X4MSO + But + X4MT))
Data_01$Total_GSL <- rowSums(Data_01[,c(4:26)])
Data_1 <- mutate(Data_01, C3ratio_emmeans= C3/(C3+C4),
                 Alk_emmeans=(OHBut+ Allyl +But)/(C3+C4),
                 OH_emmeans=(X3OHP+X4OHB)/(C3+C4),
                 MSO_emmeans=(X3MSO+X4MSO)/(C3+C4),
                 GSOH_emmeans=OHBut/(OHBut+But),
                 lc=(X7MT+X8MT+ X7MSO+X8MSO+X5MSO+X6MSO+X8MTder)/Total_GSL,
                 sc=(C3+C4)/Total_GSL,
                 GSOX_emmeans=(X3MT+X4MT)/(C3+C4),
                 Alk_OH_ratio=Alk_emmeans/(OH_emmeans+Alk_emmeans)) 

Data_1$Ref <- NA

Data_1$Ref[which(Data_1$CS == "CS76778")] <- "Col"
Data_1$Ref[which(Data_1$CS == "CS77021")] <- "Ler"
Data_1$Ref[which(Data_1$CS == "CS76789")] <- "Cvi"
Data_1$Ref[is.na(Data_1$Ref)] <- "Accession"


Data_1[is.na(Data_1)] <- 0 


#Excluding accessions:
Data_1<-Data_1[!(Data_1$Country=="AFG"),]
Data_1<-Data_1[!(Data_1$Country=="USA"),]
Data_1<-Data_1[!(Data_1$Country=="CAN"),]
Data_1<-Data_1[!(Data_1$Country=="CHN"),]
Data_1<-Data_1[!(Data_1$Country=="CPV"),]
Data_1<-Data_1[!(Data_1$Country=="UZB"),]
Data_1<-Data_1[!(Data_1$Country=="TJK"),]
Data_1<-Data_1[!(Data_1$Country=="IND"),]
Data_1<-Data_1[!(Data_1$Country=="JPN"),]
Data_1<-Data_1[!(Data_1$Country=="KAZ"),]
Data_1<-Data_1[!(Data_1$Country=="KGZ"),]
Data_1<-Data_1[!(Data_1$Country=="MAR"),]

Data_1<-Data_1[!(Data_1$Name == "Bijisk-4"),]
Data_1<-Data_1[!(Data_1$Name == "Kolyv-2"),]
Data_1<-Data_1[!(Data_1$Name == "Kolyv-3"),]
Data_1<-Data_1[!(Data_1$Name == "Kly-1"),]
Data_1<-Data_1[!(Data_1$Name == "Kly-4"),]
Data_1<-Data_1[!(Data_1$Name == "Kolyv-5"),]
Data_1<-Data_1[!(Data_1$Name == "Kolyv-6"),]
Data_1<-Data_1[!(Data_1$Name == "Koz-2"),]
Data_1<-Data_1[!(Data_1$Name == "K-oze-1"),]
Data_1<-Data_1[!(Data_1$Name == "K-oze-3"),]
Data_1<-Data_1[!(Data_1$Name == "Masl-1"),]
Data_1<-Data_1[!(Data_1$Name == "Noveg-3"),]
Data_1<-Data_1[!(Data_1$Name == "Noveg-2"),]
Data_1<-Data_1[!(Data_1$Name == "Noveg-1"),]
Data_1<-Data_1[!(Data_1$Name == "Lebja-1"),]
Data_1<-Data_1[!(Data_1$Name == "Nosov-1"),]
Data_1<-Data_1[!(Data_1$Name == "Panke-1"),]
Data_1<-Data_1[!(Data_1$Name == "Rakit-1"),]
Data_1<-Data_1[!(Data_1$Name == "Rakit-3"),]
Data_1<-Data_1[!(Data_1$Name == "Basta-1"),]
Data_1<-Data_1[!(Data_1$Name == "Basta-2"),]
Data_1<-Data_1[!(Data_1$Name == "Basta-3"),]
Data_1<-Data_1[!(Data_1$Name == "Chaba-2"),]
Data_1<-Data_1[!(Data_1$Name == "Sever-1"),]
Data_1<-Data_1[!(Data_1$Name == "Balan-1"),]
Data_1<-Data_1[!(Data_1$Name == "Valm"),]
Data_1<-Data_1[!(Data_1$Name == "Stepn-1"),]
Data_1<-Data_1[!(Data_1$Name == "Stepn-2"),]
Data_1<-Data_1[!(Data_1$Name == "Adam-1"),]
Data_1<-Data_1[!(Data_1$Name == "Karag-1"),]
Data_1<-Data_1[!(Data_1$Name == "Karag-2"),]
Data_1<-Data_1[!(Data_1$Name == "Kidr-1"),]
Data_1<-Data_1[!(Data_1$Name == "Per-1"),]
Data_1<-Data_1[!(Data_1$Name == "Pi-0"),]



#Chemotyping:

#Classifying to Alk OH MSO:

Data_1$AOP <- NA

Data_1$AOP[which(Data_1$Alk_emmeans < 0.4 & Data_1$OH_emmeans > 0.55 &Data_1$MSO_emmeans<0.1)] <- "OH"
Data_1$AOP[which(Data_1$Alk_emmeans > 0.1 & Data_1$OH_emmeans < 0.55 )] <- "Alk"
Data_1$AOP[which(Data_1$Alk_emmeans < 0.1& Data_1$OH_emmeans < 0.55)] <- "MSO"


#Manual corrections:
Data_1$AOP[which(Data_1$CS == "CS76357")] <- "Alk"
Data_1$AOP[which(Data_1$CS == "CS78767")] <- "Alk"
Data_1$AOP[which(Data_1$CS == "CS77281")] <- "Alk"
Data_1$AOP[which(Data_1$CS == "CS76729")] <- "AOP23"
Data_1$AOP[which(Data_1$CS == "CS77277")] <- "AOP23"
Data_1$AOP[which(Data_1$CS == "CS76653")] <- "AOP23"
Data_1$AOP[which(Data_1$CS == "CS76935")] <- "AOP23"
Data_1$AOP[which(Data_1$CS == "CS77299")] <- "AOP23"
Data_1$AOP[which(Data_1$CS == "CS76389")] <- "AOP23"
Data_1$AOP[which(Data_1$CS == "CS77164")] <- "AOP23"
Data_1$AOP[which(Data_1$CS == "CS76437")] <- "AOP23"
Data_1$AOP[which(Data_1$CS == "CS76445")] <- "AOP23"
Data_1$AOP[which(Data_1$CS == "CS76680")] <- "AOP23"
Data_1$AOP[which(Data_1$CS == "CS77356")] <- "AOP23"
Data_1$AOP[which(Data_1$CS == "CS77139")] <- "AOP23"
Data_1$AOP[which(Data_1$CS == "CS76934")] <- "AOP23"
Data_1$AOP[which(Data_1$CS == "CS76808")] <- "AOP23"
Data_1$AOP[which(Data_1$CS == "CS76987")] <- "AOP23"
Data_1$AOP[which(Data_1$CS == "CS78778")] <- "AOP23"
Data_1$AOP[which(Data_1$CS == "CS77224")] <- "AOP23"
Data_1$AOP[which(Data_1$CS == "CS76580")] <- "AOP23"
Data_1$AOP[which(Data_1$CS == "CS76515")] <- "AOP23"
Data_1$AOP[which(Data_1$CS == "CS77038")] <- "AOP23"


#MAM classification:

qplot(Data_1$C3ratio_emmeans, geom="histogram") +theme_bw()
ggplot(data=Data_1, aes(Data_1$C3ratio_emmeans)) + geom_histogram()+theme_bw()+
  ylab("# of accessions") +
  xlab("C3/C4 ratio")


colnames(Data_1)
Data_1$Elong<-cut(Data_1$C3ratio_emmeans, c(-0.1, 0.5, 1.1))
Data_1$Elong <- Data_1$Elong
levels(Data_1$Elong) <- c("C4", "C3")


#GSOH
colnames(Data_1)
qplot(Data_1$GSOH_emmeans, geom="histogram") +theme_bw()
ggplot(data=Data_1, aes(Data_1$GSOH_emmeans)) + geom_histogram()+theme_bw()+
  ylab("# of accessions") +
  xlab("OH_Butenyl  ratio")

Data_1$GSOH_1<-cut(Data_1$GSOH, c(-0.1, 0.1, 2))
Data_1$ GSOH_1<- as.factor(Data_1$ GSOH_1)
levels(Data_1$GSOH_1) <- c("NF", "F")



Data_1$Key <- as.factor(paste(Data_1$Elong, Data_1$AOP, Data_1$GSOH_1,  sep="_"))
Data_1$Classification <- Data_1$Key
nlevels(Data_1$Key)

levels(Data_1$Classification) <- c(1,2,3,4,5,6,7,8,9,10,11,12,13)
Data_1$Classification_name <- Data_1$Classification

#Divisions by (this order): MAM  (3/4), AOP(Alk/MSO/OH), GSOH(+/-)
levels(Data_1$Classification_name) <- c("Allyl", "Allyl", "?", "?", "3MSO", 
                                        "3MSO", "3OHP", "3OHP", "OH-But", "Butenyl", "4MSO", "4MSO", "4OHB")
Data_1<-Data_1[!(Data_1$Classification_name == "?"),]

setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA")
write.csv(Data_1, file = "Master_GSL.csv")

Data_1 <- read.table(file="Master_GSL.csv", header=T, sep=",")


#PCA with traits calculates by the emmeans:
colnames(Data_1)
data_PCA_3 <- Data_1[,c(33:37,40)]

res.pca <- prcomp(data_PCA_3, center=T, scale=T)
PCscores <- as.data.frame(res.pca$x)

data_PC_3 <- cbind(Data_1, PCscores)


ggplot(data_PC_3, aes(PC1, PC2, color=Classification_name))+
  geom_point()+theme_bw()+ ggtitle("Principal Component Analysis, by classifications") +
  scale_color_manual(values = c("orange", "lightblue", "purple", "Yellow", "red", "blue", "green", "purple"))

plot_ly(data_PC_3, x = ~PC1, y = ~PC2, z = ~PC3, color = ~ Classification_name, 
        symbol = ~Ref, symbols = c('circle','x','x','x'), size=10, alpha = 0.8)%>%
  add_markers() %>% 
  layout(scene = list(xaxis = list(title = 'PC1'),
                      yaxis = list(title = 'PC2'),
                      zaxis = list(title = 'PC3'))) %>% 
  add_text(data_PC_3, x = ~PC1, y = ~PC2, z = ~PC3) 


#Creating the PCA barplot:

pca.var <- res.pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)

barplot(pca.var.per, main="Scree Plot - traits emmeans", xlab="Principal Component", ylab="Percent Variation")

# The effect of each phenotype:
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#Contribution of variables to Dim 1:
var<-get_pca_var(res.pca)
v<-var$coord
v
fviz_contrib(res.pca, choice="var", axes=1, top=10, title="Contribution of variables - traits emmeans")



Data_2 <- cbind(Data_1, PCscores)
Data_3 <- merge(Data_2, details_1, by="CS")
Data_3 <- droplevels.data.frame(Data_3)
Data_3 <- unique(Data_3)


ggplot(Data_3, aes(PC1, PC2, color=Classification_name))+
  geom_point()+theme_bw()+ ggtitle("Principal Component Analysis")+
  scale_color_manual(values = c("orange", "lightblue", "purple", "Yellow", "red", "blue", "green", "purple"))

#PCA on GSLs:
colnames(Data_1)
data_PCA_1 <- Data_1[,c(9:31)]

res.pca <- prcomp(data_PCA_1, center=T, scale=T)
PCscores <- as.data.frame(res.pca$x)

data_PC_1 <- cbind(Data_1, PCscores)
colnames(data_PC_1)


ggplot(data_PC_1, aes(PC1, PC2, color=Ref, shape=Ref))+
  geom_point()+theme_bw()+ ggtitle("Principal Component Analysis") +
  scale_color_manual(values = c("grey", "black", "black", "black"))

setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/traits, PCA and map")
ggsave("PCA.png", dpi=300)

#Creating the PCA barplot:

pca.var <- res.pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
view(pca.var.per)
barplot(pca.var.per, main="Scree Plot - GSLs", xlab="Principal Component", ylab="Percent Variation",
        names.arg = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23"))
  

# The effect of each phenotype:
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#Contribution of variables to Dim 1:
cols <- c("X7MT"="lightgrey", "X4OHB"="red", "7MSO"="lightgrey", "6MSO"="lightgrey","OHBut"="red","4MT"="red", "X3OHP"="blue", "Branched"="lightgrey",
          Allyl="blue","5MSO"="lightgrey", "3MT"="blue", But="red", "X4MSO"="red", BZO="lightgrey", "3MSO"= "blue",
          OHPentenyl= "lightgrey", "8MSO"="lightgrey","X8MT"="lightgrey", "X4OHI3M"="lightgrey","X4MOI3M"="lightgrey") 
         
cols1 <- c("lightgrey", "lightgrey","lightgrey","lightgrey", "red", "blue", "red", "lightgrey","lightgrey","lightgrey","blue", "red", "lightgrey","blue",
           "lightgrey","lightgrey","lightgrey","blue", "lightgrey","lightgrey")
           
var<-get_pca_var(res.pca)
v<-var$coord
v
fviz_contrib(res.pca, choice="var", axes=1, top=20, title="Contribution of variables - PC1", 
             fill=cols, color = "black")+
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))

fviz_contrib(res.pca, choice="var", axes=2, top=20, title="Contribution of variables - PC2", 
             fill=cols1, color = "black")+
  theme(text = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))

colnames(data_PC_1)

ggplot(data_PC_1, aes(PC1, PC2, color=Classification_name))+
  geom_point()+theme_bw()+ ggtitle("Principal Component Analysis")+
  scale_color_manual(values = c("orange", "lightblue", "purple", "Yellow", "red", "blue", "green", "purple"))

Data_2 <- merge(data_PC_1, details_1, by="CS")
Data_2 <- droplevels.data.frame(Data_2)
Data_2 <- unique(Data_2)
colnames(Data_2)

setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny")
write.csv(data_PC_1, file = "Data_PCA.csv")

#GSOH map:

colnames(Data_1)

Data_3<- Data_1[c(-347,-167),c(2:6,48:53)]
Data_3$Elong <- as.factor(Data_3$Elong)
Data_3_Elong <- split(Data_3, Data_3$Elong)
list2env(Data_3_Elong, envir=.GlobalEnv)
rm(Data_3_Elong)

Data_C3 <- droplevels.data.frame(C3)
Data_C4 <- droplevels.data.frame(C4)

Data_C4$Classification_name <- as.factor(Data_C4$Classification_name)
Data_C4_Classification_name <- split(Data_C4, Data_C4$Classification_name)
list2env(Data_C4_Classification_name, envir=.GlobalEnv)

Butenyl <- droplevels.data.frame(Butenyl)
OH-But <- droplevels.data.frame(OH-But)

Data_5<-rbind(Butenyl, `OH-But`)


#Maps:
style1<-'[
    {
        "featureType": "administrative",
        "elementType": "all",
        "stylers": [
            {
                "visibility": "off"
            }
        ]
    },
    {
        "featureType": "administrative",
        "elementType": "labels",
        "stylers": [
            {
                "visibility": "off"
            }
        ]
    },
    {
        "featureType": "administrative",
        "elementType": "labels.text.fill",
        "stylers": [
            {
                "color": "#444444"
            }
        ]
    },
    {
        "featureType": "landscape",
        "elementType": "all",
        "stylers": [
            {
                "color": "#f2f2f2"
            }
        ]
    },
    {
        "featureType": "poi",
        "elementType": "all",
        "stylers": [
            {
                "visibility": "off"
            }
        ]
    },
    {
        "featureType": "road",
        "elementType": "all",
        "stylers": [
            {
                "saturation": -100
            },
            {
                "lightness": 45
            }
        ]
    },
    {
        "featureType": "road.highway",
        "elementType": "all",
        "stylers": [
            {
                "visibility": "simplified"
            }
        ]
    },
    {
        "featureType": "road.arterial",
        "elementType": "labels.icon",
        "stylers": [
            {
                "visibility": "off"
            }
        ]
    },
    {
        "featureType": "transit",
        "elementType": "all",
        "stylers": [
            {
                "visibility": "off"
            }
        ]
    },
    {
        "featureType": "water",
        "elementType": "all",
        "stylers": [
            {
                "color": "#5d637d"
            },
            {
                "visibility": "on"
            }
        ]
    }
]'

#46bcec old ocean color

style_list<-fromJSON(style1)

create_style_string<- function(style_list){
  style_string <- ""
  for(i in 1:length(style_list)){
    if("featureType" %in% names(style_list[[i]])){
      style_string <- paste0(style_string, "feature:", 
                             style_list[[i]]$featureType, "|")      
    }
    elements <- style_list[[i]]$stylers
    a <- lapply(elements, function(x)paste0(names(x), ":", x)) %>%
      unlist() %>%
      paste0(collapse="|")
    style_string <- paste0(style_string, a)
    if(i < length(style_list)){
      style_string <- paste0(style_string, "&style=")       
    }
  }  
  # google wants 0xff0000 not #ff0000
  style_string <- gsub("#", "0x", style_string)
  return(style_string)
}

style_string <- create_style_string(style_list)
mymap<-ggmap(get_googlemap(c(lon = 18, lat = 52),zoom=4, 
                           xlim=c(-10,72),
                           ylim=c(30, 80),
                           style=style_string), extent="device")

colnames(data_PC_1)
test<-data_PC_1[,c(2,54)]
test1<-data_PC_1[c(-347,-167),]

print(mymap)+
  geom_point(data=test1, aes(x=Long, y=Lat, color=PC1), alpha = 7/10, size=3) +
  ggtitle('') +
  xlab('Longitude') +
  ylab('Latitude') +
  theme(text = element_text(size=16, face="bold")) +
  theme(axis.text=element_text(size=16,color='black'),
        axis.title=element_text(size=18,color='black')) +
  theme(plot.title = element_text(size = 26, vjust=1)) +
  theme(plot.title = element_text(size = 18,face="bold")) +
  theme(legend.key = element_rect(fill = "white")) +
  theme(legend.text = element_text(colour="black", size=16))+
  theme(legend.title = element_text(colour="black", size=18, face="bold"))+
 theme(legend.position = c(0.045,0.889))+
        scale_colour_gradient(low = "yellow", high = "red")


ggsave("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/Manuscript/Figures/More plots/PC1.jpeg", dpi = 600)




lm_PC1 <- lm(data=data_PC_1, PC1 ~ Lat + Long+ Lat*Long)
anova(lm_PC1)
lm_PC2 <- lm(data=data_PC_1, PC2 ~ Lat + Long+ Lat*Long)
anova(lm_PC2)

print(mymap)+
  geom_point(data=test1, aes(x=Long, y=Lat, color=Classification_name), alpha = 7/10, size=3) +
  ggtitle('') +
  xlab('Longitude') +
  ylab('Latitude') +
  theme(text = element_text(size=16, face="bold")) +
  theme(axis.text=element_text(size=16,color='black'),
        axis.title=element_text(size=18,color='black')) +
  theme(plot.title = element_text(size = 26, vjust=1)) +
  theme(plot.title = element_text(size = 18,face="bold")) +
  theme(legend.key = element_rect(fill = "white")) +
  theme(legend.text = element_text(colour="black", size=16))+
  theme(legend.title = element_text(colour="black", size=18, face="bold"))+
  scale_color_manual(values= c("yellow", "green2","magenta", "skyblue", "blue", "black", "red3"))

ggsave("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/Manuscript/Figures/More plots/Cheotypes.jpeg", dpi = 600)



print(mymap)+
  geom_point(data=Data_5, aes(x=Long, y=Lat, color=GSOH_1), alpha = 7/10, size=3) +
  ggtitle('') +
  xlab('Longitude') +
  ylab('Latitude') +
  theme(text = element_text(size=16, face="bold")) +
  theme(axis.text=element_text(size=16,color='black'),
        axis.title.x=element_text(size=16,color='black'),
        axis.title.y=element_text(size=16,color='black', hjust=0.22)) +
  theme(plot.title = element_text(size = 26, vjust=1)) +
  theme(plot.title = element_text(size = 18,face="bold")) +
  theme(legend.key = element_rect(fill = "white")) +
  theme(legend.text = element_text(colour="black", size=16))+
  theme(legend.title = element_text(colour="black", size=16, face="bold"))+
  theme(legend.position = c(0.04,0.95))+
  labs(colour="GSOH")+
  scale_color_manual(values = c("sienna2", "black"))

ggsave("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/Manuscript/Figures/More plots/GSOH.jpeg", dpi = 600)

print(mymap)+
  geom_point(data=test1, aes(x=Long, y=Lat, color=AOP), alpha = 7/10, size=3) +
  ggtitle('') +
  xlab('Longitude') +
  ylab('Latitude') +
  theme(text = element_text(size=16, face="bold")) +
  theme(axis.text=element_text(size=16,color='black'),
        axis.title.x=element_text(size=16,color='black'),
        axis.title.y=element_text(size=16,color='black', hjust=0.22)) +
  theme(plot.title = element_text(size = 26, vjust=1)) +
  theme(plot.title = element_text(size = 18,face="bold")) +
  theme(legend.key = element_rect(fill = "white")) +
  theme(legend.text = element_text(colour="black", size=16))+
  theme(legend.title = element_text(colour="black", size=16, face="bold"))+
  theme(legend.position = c(0.067,0.93))+
  scale_color_manual(values = c("magenta","green3", "yellow"))

ggsave("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/Manuscript/Figures/More plots/AOP.jpeg", dpi = 600)

print(mymap)+
  geom_point(data=test1, aes(x=Long, y=Lat, color=Elong), alpha = 7/10, size=3) +
  ggtitle('') +
  xlab('Longitude') +
  ylab('Latitude') +
  theme(text = element_text(size=16, face="bold")) +
  theme(axis.text=element_text(size=16,color='black'),
        axis.title.x=element_text(size=16,color='black'),
        axis.title.y=element_text(size=16,color='black', hjust=0.22)) +
  theme(plot.title = element_text(size = 26, vjust=1)) +
  theme(plot.title = element_text(size = 18,face="bold")) +
  theme(legend.key = element_rect(fill = "white")) +
  theme(legend.text = element_text(colour="black", size=16))+
  theme(legend.title = element_text(colour="black", size=16, face="bold"))+
  theme(legend.position = c(0.09,0.94))+
  labs(colour="Chain length")+
  scale_color_manual(values = c("blue","red"))

ggsave("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/Manuscript/Figures/More plots/Elong.jpeg", dpi = 600)





scale_color_gradientn(colours = rainbow(9))
  scale_colour_gradient(low = "blue", high = "red")
  
scale_colour_gradientn(colours = terrain.colors(10))  
  



#Grouping by the mountains:
test1$Mountain <- NA
test1$Mountain[which(test1$Country == "ESP")] <- "South"
test1$Mountain[which(test1$Country == "POR")] <- "South"
test1$Mountain[which(test1$Country == "ITA")] <- "South"
test1$Mountain[which(test1$Country == "GRC")] <- "South"
test1$Mountain[which(test1$Country == "BUL")] <- "South"
test1$Mountain[which(test1$Country == "CRO")] <- "South"
test1$Mountain[which(test1$Country == "SRB")] <- "South"
test1$Mountain[which(test1$Country == "ROU")] <- "South"
test1$Mountain[which(test1$Country == "SVK")] <- "South"
test1$Mountain[which(test1$Country == "LBN")] <- "South"
test1$Mountain[which(test1$Country == "GEO")] <- "South"
test1$Mountain[which(test1$Country == "ARM")] <- "South"
test1$Mountain[which(test1$Country == "Gr-5")] <- "South"
test1$Mountain[which(test1$Country == "Gr-1")] <- "South"

test1$Mountain[is.na(test1$Mountain)] <- "North"


print(mymap)+
  geom_point(data=test1, aes(x=Long, y=Lat, color=Mountain), alpha = 7/10, size=3) +
  ggtitle('') +
  xlab('Longitude') +
  ylab('Latitude') +
  theme(text = element_text(size=16, face="bold")) +
  theme(axis.text=element_text(size=16,color='black'),
        axis.title=element_text(size=16,color='black')) +
  theme(plot.title = element_text(size = 26, vjust=1)) +
  theme(plot.title = element_text(size = 18,face="bold")) +
  theme(legend.key = element_rect(fill = "white")) +
  theme(legend.text = element_text(colour="black", size=16))+
  theme(legend.title = element_text(colour="black", size=16, face="bold"))+
   theme(legend.position = c(0.075,0.938))+
  labs(colour="Geography")+
  scale_color_manual(values = c("chartreuse4", "deeppink2"))


ggsave("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/Manuscript/Figures/More plots/Geography.jpeg", dpi = 600)










basemap <- get_googlemap(c(lon = 17, lat = 52),zoom=4, 
                         xlim=c(-22,60),
                         ylim=c(30, 80),
                         #maptype = 'hybrid',
                         #maptype = 'terrain',
                         #maptype = 'satellite',
                         maptype = 'roadmap',
                         color="bw",
                         key="AIzaSyAYVck1dRnEJY0Sfzsb9i5K9gWqlwExITI")

ggmap(basemap) + 
  geom_point(data=Data_3, aes(x=Long, y=Lat, color=Classification_name), alpha = 5/10) +
  ggtitle('') +
xlab('') +
  ylab('') +
  theme(text = element_text(size=18)) +
  theme(axis.text=element_text(size=8,color='black'),
        axis.title=element_text(size=20,color='black')) +
  theme(plot.title = element_text(size = 26, vjust=1)) +
  theme(plot.title = element_text(size = 18,face="bold")) +
  theme(legend.key = element_rect(fill = "white")) +
  theme(legend.text = element_text(colour="black", size=8))+
  theme(legend.title = element_text(colour="black", size=10, face="bold"))+
  scale_color_manual(values = c("yellow", "blue", "lightblue", "orange", "red", "turquoise", "green", "purple"))

ggmap(basemap) + 
  geom_point(data=Data_3, aes(x=Long, y=Lat, color=PC1), alpha = 5/10) +
  ggtitle('') +
  xlab('') +
  ylab('') +
  theme(text = element_text(size=18)) +
  theme(axis.text=element_text(size=8,color='black'),
        axis.title=element_text(size=20,color='black')) +
  theme(plot.title = element_text(size = 26, vjust=1)) +
  theme(plot.title = element_text(size = 18,face="bold")) +
  theme(legend.key = element_rect(fill = "white")) +
  theme(legend.text = element_text(colour="black", size=8))+
  theme(legend.title = element_text(colour="black", size=10, face="bold"))+
scale_color_gradientn(colours = rainbow(5))



basemap <- get_googlemap(c(lon = 35, lat = 40),zoom=3, 
                         xlim=c(-22,78),
                         ylim=c(24, 80),
                         #maptype = 'hybrid',
                         #maptype = 'terrain',
                         #maptype = 'satellite',
                         maptype = 'roadmap',
                         color="bw",
                         key="AIzaSyAYVck1dRnEJY0Sfzsb9i5K9gWqlwExITI")

basemap <- get_googlemap(c(lon = 18, lat = 52),zoom=4, 
                         xlim=c(-10,72),
                         ylim=c(30, 80),
                         
                         #maptype = 'hybrid',
                         #maptype = 'terrain',
                         #maptype = 'satellite',
                         maptype = 'roadmap',
                         key="AIzaSyAYVck1dRnEJY0Sfzsb9i5K9gWqlwExITI")

ggmap(basemap) + 
  geom_point(data=Data_3, aes(x=Long, y=Lat, color=Classification_name), alpha = 5/10) +
  ggtitle('') +
  xlab('') +
  ylab('') +
  theme(text = element_text(size=18)) +
  theme(axis.text=element_text(size=8,color='black'),
        axis.title=element_text(size=20,color='black')) +
  theme(plot.title = element_text(size = 26, vjust=1)) +
  theme(plot.title = element_text(size = 18,face="bold")) +
  theme(legend.key = element_rect(fill = "white")) +
  theme(legend.text = element_text(colour="black", size=8))+
  theme(legend.title = element_text(colour="black", size=10, face="bold"))+
  scale_color_manual(values = c("yellow", "blue", "lightblue", "orange", "red", "turquoise", "green", "purple"))

#Costumized map:
#https://snazzymaps.com/editor
#https://mapstyle.withgoogle.com/

style1<-'[
    {
        "featureType": "administrative",
        "elementType": "all",
        "stylers": [
            {
                "visibility": "off"
            }
        ]
    },
    {
        "featureType": "administrative",
        "elementType": "labels",
        "stylers": [
            {
                "visibility": "off"
            }
        ]
    },
    {
        "featureType": "administrative",
        "elementType": "labels.text.fill",
        "stylers": [
            {
                "color": "#444444"
            }
        ]
    },
    {
        "featureType": "landscape",
        "elementType": "all",
        "stylers": [
            {
                "color": "#f2f2f2"
            }
        ]
    },
    {
        "featureType": "poi",
        "elementType": "all",
        "stylers": [
            {
                "visibility": "off"
            }
        ]
    },
    {
        "featureType": "road",
        "elementType": "all",
        "stylers": [
            {
                "saturation": -100
            },
            {
                "lightness": 45
            }
        ]
    },
    {
        "featureType": "road.highway",
        "elementType": "all",
        "stylers": [
            {
                "visibility": "simplified"
            }
        ]
    },
    {
        "featureType": "road.arterial",
        "elementType": "labels.icon",
        "stylers": [
            {
                "visibility": "off"
            }
        ]
    },
    {
        "featureType": "transit",
        "elementType": "all",
        "stylers": [
            {
                "visibility": "off"
            }
        ]
    },
    {
        "featureType": "water",
        "elementType": "all",
        "stylers": [
            {
                "color": "#46bcec"
            },
            {
                "visibility": "on"
            }
        ]
    }
]'
style_list<-fromJSON(style1)

create_style_string<- function(style_list){
  style_string <- ""
  for(i in 1:length(style_list)){
    if("featureType" %in% names(style_list[[i]])){
      style_string <- paste0(style_string, "feature:", 
                             style_list[[i]]$featureType, "|")      
    }
    elements <- style_list[[i]]$stylers
    a <- lapply(elements, function(x)paste0(names(x), ":", x)) %>%
      unlist() %>%
      paste0(collapse="|")
    style_string <- paste0(style_string, a)
    if(i < length(style_list)){
      style_string <- paste0(style_string, "&style=")       
    }
  }  
  # google wants 0xff0000 not #ff0000
  style_string <- gsub("#", "0x", style_string)
  return(style_string)
}

style_string <- create_style_string(style_list)
mymap<-ggmap(get_googlemap(c(lon = 18, lat = 52),zoom=4, 
                           xlim=c(-10,72),
                           ylim=c(30, 80),
                           style=style_string), extent="device")
print(mymap)+
  geom_point(data=Data_3, aes(x=Long, y=Lat, color=Classification_name), alpha = 5/10) +
  ggtitle('') +
  xlab('Longitude') +
  ylab('Latitude') +
  theme(text = element_text(size=18)) +
  theme(axis.text=element_text(size=8,color='black'),
        axis.title=element_text(size=12,color='black')) +
  theme(plot.title = element_text(size = 26, vjust=1)) +
  theme(plot.title = element_text(size = 18,face="bold")) +
  theme(legend.key = element_rect(fill = "white")) +
  theme(legend.text = element_text(colour="black", size=8))+
  theme(legend.title = element_text(colour="black", size=10, face="bold"))+
  scale_color_manual(values = c("yellow", "blue", "grey", "orange", "red", "black", "green", "purple"))


ggsave(mymap, filename="mymap.png")


#133 accession from 360 collection:
setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/Stat/emmeans")
Data_360 <- read.table(file="Data_360.csv", header=T, sep=",")

colnames(Data_3)
Data_4 <-Data_3[, c(1,57,58)] 
Data_360_1<- merge(Data_360, Data_4, by="CS")


basemap <- get_googlemap(c(lon = 18, lat = 52),zoom=4, 
                         xlim=c(-10,72),
                         ylim=c(30, 80),
                         #maptype = 'hybrid',
                         #maptype = 'terrain',
                         #maptype = 'satellite',
                         maptype = 'roadmap',
                         key="AIzaSyAYVck1dRnEJY0Sfzsb9i5K9gWqlwExITI")
ggmap(basemap)+ 
  geom_point(data=Data_360_1, aes(x=Long, y=Lat, color=Classification_name), alpha = 5/10) +
  ggtitle('') +
  xlab('') +
  ylab('') +
  theme(text = element_text(size=18)) +
  theme(axis.text=element_text(size=8,color='black'),
        axis.title=element_text(size=20,color='black')) +
  theme(plot.title = element_text(size = 26, vjust=1)) +
  theme(plot.title = element_text(size = 18,face="bold")) +
  theme(legend.key = element_rect(fill = "white")) +
  theme(legend.text = element_text(colour="black", size=8))+
  theme(legend.title = element_text(colour="black", size=10, face="bold"))+
  scale_color_manual(values = c("grey", "orange", "red", "green",  "yellow", "black", "blue"))




#PCA without C7 C8:
colnames(Data_1)
data_PCA_3 <- Data_1[,c(9:21, 23,24,26,28:29)]

res.pca <- prcomp(data_PCA_3, center=T, scale=T)
PCscores <- as.data.frame(res.pca$x)

data_PC_3 <- cbind(Data_1, PCscores)


ggplot(data_PC_3, aes(PC1, PC2, color=Classification_name))+
  geom_point()+theme_bw()+ ggtitle("Principal Component Analysis, by classifications") +
  scale_color_manual(values = c("orange", "lightblue", "purple", "Yellow", "red", "blue", "green", "purple"))

plot_ly(data_PC_3, x = ~PC1, y = ~PC2, z = ~PC3, color = ~ Classification_name, 
        symbol = ~Ref, symbols = c('circle','x','x','x'), size=10, alpha = 0.8)%>%
  add_markers() %>% 
  layout(scene = list(xaxis = list(title = 'PC1'),
                      yaxis = list(title = 'PC2'),
                      zaxis = list(title = 'PC3'))) %>% 
  add_text(data_PC_3, x = ~PC1, y = ~PC2, z = ~PC3) 


#Creating the PCA barplot:

pca.var <- res.pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)

barplot(pca.var.per, main="Scree Plot - traits emmeans", xlab="Principal Component", ylab="Percent Variation")

# The effect of each phenotype:
fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#Contribution of variables to Dim 1:
var<-get_pca_var(res.pca)
v<-var$coord
v
fviz_contrib(res.pca, choice="var", axes=2, top=10, title="Contribution of variables - traits emmeans")



Data_2 <- cbind(Data_1, PCscores)
Data_3 <- merge(Data_2, details_1, by="CS")
Data_3 <- droplevels.data.frame(Data_3)
Data_3 <- unique(Data_3)


ggplot(Data_3, aes(PC1, PC2, color=Classification_name))+
  geom_point()+theme_bw()+ ggtitle("Principal Component Analysis")+
  scale_color_manual(values = c("orange", "lightblue", "purple", "Yellow", "red", "blue", "green", "purple"))

print(mymap)+
  geom_point(data=Data_3, aes(x=Long.x, y=Lat.x, color=PC1), alpha = 5/10, size=2) +
  ggtitle('') +
  xlab('Longitude') +
  ylab('Latitude') +
  theme(text = element_text(size=18)) +
  theme(axis.text=element_text(size=8,color='black'),
        axis.title=element_text(size=12,color='black')) +
  theme(plot.title = element_text(size = 26, vjust=1)) +
  theme(plot.title = element_text(size = 18,face="bold")) +
  theme(legend.key = element_rect(fill = "white")) +
  theme(legend.text = element_text(colour="black", size=8))+
  theme(legend.title = element_text(colour="black", size=10, face="bold"))+
  scale_color_gradient2(low = 'yellow', mid = 'red', high = 'red')
 

