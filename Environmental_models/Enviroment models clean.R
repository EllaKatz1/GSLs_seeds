setwd("C:/Users/Ella Katz/Desktop/Post-doc/Projects/Sarah Turner/Analyses/PCA/Enviroment")
Accessions_data <- read.table(file="Data_traits_short.csv", header=T, sep=",")
Enviromental_data <- read.table(file="shiny climatesd short.csv", header=T, sep=",")

colnames(Data_1)


Data_1 <- merge(Accessions_data, Enviromental_data, by="CS")

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

#Excluding these??
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


Data_1<-Data_1[!(Data_1$Classification_name == "?"),]





colnames(Data_1)
#Changing to 0/1:
Data_1$AOP_Status <- NA
Data_1$AOP_Status[which(Data_1$AOP == "Alk")] <- "-1"
Data_1$AOP_Status[which(Data_1$AOP == "OH")] <- "1"
Data_1$AOP_Status[which(Data_1$AOP == "MSO")] <- "0"
Data_1$AOP_Status[which(Data_1$AOP == "AOP23")] <- "NA"

Data_1<-Data_1[!(Data_1$AOP_Status=="NA"),]


Data_1$MAM_Status <- NA
Data_1$MAM_Status[which(Data_1$MAM == "C3")] <- "0"
Data_1$MAM_Status[which(Data_1$MAM == "C4")] <- "1"


Data_1$MAM_GSOH <- as.factor(paste(Data_1$MAM, Data_1$GSOH, sep="_"))

Data_1$GSOH_Status <- NA
Data_1$GSOH_Status[which(Data_1$MAM_GSOH == "C4_F")] <- "1"
Data_1$GSOH_Status[is.na(Data_1$GSOH_Status)] <-"0"

Data_1$MAM_Status<- as.numeric(as.character(Data_1$MAM_Status))
Data_1$GSOH_Status<- as.numeric(as.character(Data_1$GSOH_Status))
Data_1$AOP_Status<- as.numeric(as.character(Data_1$AOP_Status))

#Grouping by geography:
Data_1$GEO <- NA
Data_1$GEO[which(Data_1$Country == "ESP")] <- "Iberian_Peninsula"
Data_1$GEO[which(Data_1$Country == "POR")] <- "Iberian_Peninsula"

Data_1$GEO[is.na(Data_1$GEO)] <- "Europe"

#Separating the GEO:

Data_1$GEO_1 <- paste("G", Data_1$GEO, sep="")
Data_1$GEO_1 <- as.factor(Data_1$GEO_1)
Data_1_GEO <- split(Data_1, Data_1$GEO_1)
list2env(Data_1_GEO, envir=.GlobalEnv)
rm(Data_1_GEO)


#Stat just for Iberian penincula:

Data_1GIberian_Peninsula <- droplevels.data.frame(GIberian_Peninsula)
Data_1GEurope <- droplevels.data.frame(GEurope)

#Grouping by the mountains:
Data_1$Mountain <- NA
Data_1$Mountain[which(Data_1$Country == "ESP")] <- "South"
Data_1$Mountain[which(Data_1$Country == "POR")] <- "South"
Data_1$Mountain[which(Data_1$Country == "ITA")] <- "South"
Data_1$Mountain[which(Data_1$Country == "GRC")] <- "South"
Data_1$Mountain[which(Data_1$Country == "BUL")] <- "South"
Data_1$Mountain[which(Data_1$Country == "CRO")] <- "South"
Data_1$Mountain[which(Data_1$Country == "SRB")] <- "South"
Data_1$Mountain[which(Data_1$Country == "ROU")] <- "South"
Data_1$Mountain[which(Data_1$Country == "SVK")] <- "South"
Data_1$Mountain[which(Data_1$Country == "LBN")] <- "South"
Data_1$Mountain[which(Data_1$Country == "GEO")] <- "South"
Data_1$Mountain[which(Data_1$Country == "ARM")] <- "South"
Data_1$Mountain[which(Data_1$Name == "Gr-5")] <- "South"
Data_1$Mountain[which(Data_1$Name == "Gr-1")] <- "South"

Data_1$Mountain[is.na(Data_1$Mountain)] <- "North"

#Separating the Mountain:

Data_1$Mountain <- as.factor(Data_1$Mountain)
Data_1_Mountain <- split(Data_1, Data_1$Mountain)
list2env(Data_1_Mountain, envir=.GlobalEnv)
rm(Data_1_Mountain)


Data_North <- droplevels.data.frame(North)
Data_South <- droplevels.data.frame(South)



print(mymap)+
  geom_point(data=Data_1, aes(x=Long, y=Lat, color=Classification_name), alpha = 8/10, size=2) +
  ggtitle('') +
  xlab('Longtitude') +
  ylab('Latitude') +
  theme(text = element_text(size=18)) +
  theme(axis.text=element_text(size=8,color='black'),
        axis.title=element_text(size=12,color='black')) +
  theme(plot.title = element_text(size = 26, vjust=1)) +
  theme(plot.title = element_text(size = 18,face="bold")) +
  theme(legend.key = element_rect(fill = "white")) +
  theme(legend.text = element_text(colour="black", size=8))+
  theme(legend.title = element_text(colour="black", size=10, face="bold"))+
  scale_color_manual(values = c("yellow", "green2", "magenta", "skyblue", "blue", "black", "red3"))

print(mymap)+
  geom_point(data=Data_1, aes(x=Long, y=Lat, color=Mountain), alpha = 7/10, size=3) +
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
  theme(legend.position = c(0.098,0.935))+
  scale_color_manual(name="Geography", values= c("chartreuse4", "maroon1"))

ggsave("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/Manuscript/Figures/More plots/Geography.jpeg", dpi = 600)



colnames(Data_1)

ggplot(Data_1, aes(x=factor(1), fill=Classification_name))+
  geom_bar(width = 1)+
  coord_polar("y")+ theme_void()+
  labs(x = NULL, y = NULL, fill = Classification_name, title = "Chemotypes - Iberian Peninsula")+
    theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black"))+
  theme(plot.margin = unit(c(1, 9, 8, 5), "cm"))+
  scale_color_manual(values = c("yellow", "green2", "magenta", "skyblue", "blue", "black", "red3"))


#Logistic regression:
install.packages("aod")
library(aod)

Data_1GIberian_Peninsula$Classification_name <- factor(Data_1GIberian_Peninsula$Classification_name)
mylogit <- glm(WC2_BIO7 ~ MAM + AOP + GSOH  +Classification_name, data = Data_1GIberian_Peninsula, family = "binomial")




Enviromental_data_long <- read.table(file="shiny climatesd.csv", header=T, sep=",")
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
mymap<-ggmap(get_googlemap(c(lon = 18, lat = 52),zoom=2, 
                           xlim=c(-10,72),
                           ylim=c(30, 80),
                           style=style_string), extent="device")
colnames(Data_1)

print(mymap)+
  geom_point(data=Data_1, aes(x=Long, y=Lat, color=SRTM_elevation), alpha = 7/10, size=2) +
  ggtitle('') +
  xlab('Longtitude') +
  ylab('Latitude') +
  theme(text = element_text(size=18)) +
  theme(axis.text=element_text(size=8,color='black'),
        axis.title=element_text(size=12,color='black')) +
  theme(plot.title = element_text(size = 26, vjust=1)) +
  theme(plot.title = element_text(size = 18,face="bold")) +
  theme(legend.key = element_rect(fill = "white")) +
  theme(legend.text = element_text(colour="black", size=8))+
  theme(legend.title = element_text(colour="black", size=10, face="bold"))+
  scale_color_gradientn(colours = rainbow(9))




print(mymap)+
  geom_point(data=Data_1, aes(x=Long, y=Lat, color=Classification_name), alpha = 7/10, size=2) +
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
  scale_color_manual(values = c("orange", "yellow", "green2", "magenta", "skyblue", "blue", "black", "red3"))


print(mymap)+
  geom_point(data=Data_1, aes(x=Long, y=Lat, color=Classification_name), alpha = 7/10, size=2) +
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
  scale_color_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a", "#e31a1c", "#fb9a99", "#33a02c", "#fdbf6f", "#ff7f00"))


print(mymap)+
  geom_point(data=Data_1, aes(x=Long, y=Lat, color=Classification_name), alpha = 7/10, size=2) +
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
  scale_color_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f", "#e5c494", "#b3b3b3"))

print(mymap)+
  geom_point(data=Data_1, aes(x=Long, y=Lat, color=Classification_name), alpha = 7/10, size=2) +
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
  scale_color_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ffff33", "#ff7f00", "#a65628", "#f781bf"))


#Combinations:
LAI_Spring
SRTM_elevation
CHELSA_Interann_Temp
CHELSA_BIO4
CHELSA_BIO6
CHELSA_BIO7
CHELSA_BIO11
WC2_BIO7
WC2_BIO6
WC2_BIO12
WC2_BIO13
WC2_BIO16
WC2_BIO19

colnames(Data_1)
colnames(Data_North)
#
a <- manova(cbind(WC2_BIO5,WC2_BIO6, WC2_BIO13, WC2_BIO14, Distance_to_the_coast) ~ Classification_name, data = Data_North)
summary(a)
summary.aov(a)
cld(emmeans(a, ~Classification_name))

ggplot(data=Data_1, aes(Classification_name, WC2_BIO5, fill=Classification_name))+ 
  theme_minimal() + 
  geom_bar(aes(fill=Classification_name), stat = "summary", fun.y = "mean", position = position_dodge(), width=0.7)+ 
  geom_jitter(aes(colour = Classification_name), position = position_jitter(width = 0.1), alpha = 0.2) +  
  ggtitle("")+
  scale_fill_manual(values = c("orange", "yellow", "green2", "magenta", "skyblue", "blue", "black", "red3"))+
  ylab("WC2_BIO5") +
  xlab("Chemotype") +
  theme(plot.margin = unit(c(1, 8, 1, 2), "cm"), axis.text.x = element_text(angle = 45))

colnames(Data_1)
b <- manova(cbind(MAM_Status, AOP_Status, GSOH_Status) ~ WC2_BIO5+WC2_BIO6+ WC2_BIO13+ WC2_BIO14 + Distance_to_the_coast , data = Data_South)
summary(b)
summary.aov(b)


c <- lm(cbind(MAM_Status, AOP_Status, GSOH_Status) ~ WC2_BIO5+WC2_BIO6+ WC2_BIO13+ WC2_BIO14 + Distance_to_the_coast+group+Mountain 
        + Mountain *WC2_BIO5+ Mountain *WC2_BIO6 + Mountain *WC2_BIO13 + Mountain *WC2_BIO14+ Mountain *Distance_to_the_coast +Mountain*group, data = Data_1)
anova(c)
summary.aov(c)

c1 <- lm(cbind(MAM_Status, AOP_Status, GSOH_Status) ~ WC2_BIO5+WC2_BIO6+ WC2_BIO13+ WC2_BIO14 + Distance_to_the_coast+Mountain 
        + Mountain *WC2_BIO5+ Mountain *WC2_BIO6 + Mountain *WC2_BIO13 + Mountain *WC2_BIO14+ Mountain *Distance_to_the_coast, data = Data_1)
anova(c1)
summary.aov(c1)

c2 <- lm(cbind(MAM_Status, AOP_Status, GSOH_Status) ~ WC2_BIO5+WC2_BIO6+ WC2_BIO13+ WC2_BIO14 + Distance_to_the_coast+group, data = Data_South)
anova(c2)
summary.aov(c2)

ggplot(Data_1, aes(x=factor(1), fill=Classification_name))+
  geom_bar(width = 1)+
  coord_polar("y")+ theme_void()+
  labs(x = NULL, y = NULL, fill = NULL, title = "Chemotypes - All Data")+
  scale_fill_manual(values = c("yellow", "green2", "magenta", "skyblue", "blue", "black", "red3"))+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black"))+
  theme(plot.margin = unit(c(1, 9, 8, 5), "cm"))

#Violin plots:

ggplot(data=Data_2North, aes(MAM, Temp_warm, fill=MAM))+ 
  theme_classic()+ 
  geom_violin(aes(fill=MAM))+ 
  geom_jitter(colour="grey23", position = position_jitter(width = 0.1), alpha = 0.5)+  
  ggtitle("South")+ theme(plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values = c("blue", "red"))+
  geom_boxplot(width=0.15, fill="grey") +
   ylab("Max temperature of warmest month (?C)") +
  xlab("C chain length")+
  ylim(8,32)+
  theme(plot.margin = unit(c(1, 1, 1, 5), "cm"))

ggplot(data=Data_South, aes(Classification_name, WC2_BIO6, fill=Classification_name))+ 
  theme_classic()+ 
  geom_violin(aes(fill=Classification_name))+ 
  geom_jitter(colour="grey23", position = position_jitter(width = 0.1), alpha = 0.5)+  
  ggtitle("South")+ theme(plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values = c("yellow", "green2", "magenta", "skyblue", "blue", "black", "red3"))+
  geom_boxplot(width=0.15, fill="grey") +
  ylab("Min temperature of coldest month (?C)") +
  xlab("Chemotype")+
  ylim(-27,12)+
  theme(plot.margin = unit(c(1, 1, 1, 5), "cm"))

ggplot(data=Data_South, aes(Classification_name, WC2_BIO13, fill=Classification_name))+ 
  theme_classic()+ 
  geom_violin(aes(fill=Classification_name))+ 
  geom_jitter(colour="grey23", position = position_jitter(width = 0.1), alpha = 0.5)+  
  ggtitle("South")+ theme(plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values = c("yellow", "green2", "magenta", "skyblue", "blue", "black", "red3"))+
  geom_boxplot(width=0.15, fill="grey") +
  ylab("Precipitation of wettest month (mm)") +
  xlab("Chemotype")+
  ylim(30,300)+
  theme(plot.margin = unit(c(1, 1, 1, 5), "cm"))

ggplot(data=Data_South, aes(Classification_name, WC2_BIO14, fill=Classification_name))+ 
  theme_classic()+ 
  geom_violin(aes(fill=Classification_name))+ 
  geom_jitter(colour="grey23", position = position_jitter(width = 0.1), alpha = 0.5)+  
  ggtitle("South")+ theme(plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values = c("yellow", "green2", "magenta", "skyblue", "blue", "black", "red3"))+
  geom_boxplot(width=0.15, fill="grey") +
  ylab("Precipitation of driest month (mm)") +
  xlab("Chemotype")+
ylim(0,125)+
  theme(plot.margin = unit(c(1, 1, 1, 5), "cm"))


ggplot(data=Data_South, aes(Classification_name, Distance_to_the_coast, fill=Classification_name))+ 
  theme_classic()+ 
  geom_violin(aes(fill=Classification_name))+ 
  geom_jitter(colour="grey23", position = position_jitter(width = 0.1), alpha = 0.5)+  
  ggtitle("South")+ theme(plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values = c("yellow", "green2", "magenta", "skyblue", "blue", "black", "red3"))+
  geom_boxplot(width=0.15, fill="grey") +
  ylab("Distance to the coast (km)") +
  xlab("Chemotype")+
  ylim(0,1900)+
  theme(plot.margin = unit(c(1, 1, 1, 5), "cm"))


#Excluding 4OHB and Butenyl, 3MSO in the south:
Data_1<-Data_1[!(Data_1$Classification_name=="4OHB"),]
Data_1<-Data_1[!(Data_1$Classification_name=="Butenyl"),]

Data_North<-Data_North[!(Data_North$Classification_name=="4OHB"),]
Data_North<-Data_North[!(Data_North$Classification_name=="Butenyl"),]

Data_South<-Data_South[!(Data_South$Classification_name=="4OHB"),]
Data_South<-Data_South[!(Data_South$Classification_name=="Butenyl"),]
Data_South<-Data_South[!(Data_South$Classification_name=="3MSO"),]

ggplot(data=Data_North, aes(Classification_name, WC2_BIO5, fill=Classification_name))+ 
  theme_classic()+ 
  geom_violin(aes(fill=Classification_name))+ 
  geom_jitter(colour="grey23", position = position_jitter(width = 0.1), alpha = 0.5)+  
  ggtitle("South")+ theme(plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values = c( "blue", "red3"))+
  geom_boxplot(width=0.15, fill="grey") +
  ylab("Max temperature of warmest month (?C)") +
  xlab("Chemotype")+
  ylim(8,32)+
  theme(plot.margin = unit(c(1, 1, 1, 5), "cm"))

ggplot(data=Data_2South, aes(Classification_name, WC2_BIO5, fill=Classification_name))+ 
  theme_classic()+ 
  geom_violin(aes(fill=Classification_name))+ 
  geom_jitter(colour="grey23", position = position_jitter(width = 0.1), alpha = 0.5)+  
  ggtitle("South")+ theme(plot.title = element_text(size=20, hjust=0.5, vjust = - 8, color="black", face="bold"))+
  scale_fill_manual(values = c( "green2", "magenta", "blue", "red3"))+
  geom_boxplot(width=0.15, fill="white") +
  ylab("Max temperature of warmest month (?C)") +
  ylab("Precipitation of driest month (mm)") +
  theme(axis.title=element_text(size=19,face="bold"),
        axis.text.x = element_text(size = 16, color="black", face="bold"),
        axis.text.y = element_text(size = 16, color="black", face="bold"),
        legend.position = "none")+
  xlab("Chemotype")+
  ylim(8,32)+
  theme(plot.margin = unit(c(1, 5, 1, 1), "cm"))

setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/Manuscript/Figures/Violins")

ggsave("temp warm south.tiff", dpi=600, compression = 'lzw')



ggplot(data=Data_1, aes(Classification_name, WC2_BIO6, fill=Classification_name))+ 
  theme_classic()+ 
  geom_violin(aes(fill=Classification_name))+ 
  geom_jitter(colour="grey23", position = position_jitter(width = 0.1), alpha = 0.5)+  
   scale_fill_manual(values = c("yellow", "green2", "magenta", "blue", "red3"))+
  geom_boxplot(width=0.15, fill="white") +
  ylab("Min temperature of coldest month (?C)") +
  theme(axis.title=element_text(size=19,face="bold"),
        axis.text.x = element_text(size = 16, color="black", face="bold"),
        axis.text.y = element_text(size = 16, color="black", face="bold"),
        legend.position = "none")+
  xlab("Chemotype")+
  ylim(-27,12)+
  theme(plot.margin = unit(c(1, 5, 1, 1), "cm"))

ggsave("temp cold all.tiff", dpi=600, compression = 'lzw')

ggplot(data=Data_South, aes(Classification_name, WC2_BIO13, fill=Classification_name))+ 
  theme_classic()+ 
  geom_violin(aes(fill=Classification_name))+ 
  geom_jitter(colour="grey23", position = position_jitter(width = 0.1), alpha = 0.5)+  
  scale_fill_manual(values = c( "green2", "magenta", "blue", "red3"))+
  geom_boxplot(width=0.15, fill="white") +
  ylab("Precipitation of wettest month (mm)") +
  theme(axis.title=element_text(size=19,face="bold"),
        axis.text.x = element_text(size = 16, color="black", face="bold"),
        axis.text.y = element_text(size = 16, color="black", face="bold"),
        legend.position = "none")+
   xlab("Chemotype")+
  ylim(30,300)+
  theme(plot.margin = unit(c(1, 5, 1, 1), "cm"))


ggsave("prec wet south.tiff", dpi=600, compression = 'lzw')


ggplot(data=Data_North, aes(Classification_name, WC2_BIO14, fill=Classification_name))+ 
  theme_classic()+ 
  geom_violin(aes(fill=Classification_name))+ 
  geom_jitter(colour="grey23", position = position_jitter(width = 0.1), alpha = 0.5)+  
   scale_fill_manual(values = c("yellow", "green2", "magenta","blue", "red3"))+
  geom_boxplot(width=0.1, fill="white") +
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))+
  ylab("Precipitation of driest month (mm)") +
  theme(axis.title=element_text(size=19,face="bold"),
        axis.text.x = element_text(size = 16, color="black", face="bold"),
        axis.text.y = element_text(size = 16, color="black", face="bold"),
        legend.position = "none")+
  xlab("Chemotype")+
  ylim(0,125)+
  theme(plot.margin = unit(c(1, 5, 1, 1), "cm"))


setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/Manuscript/Figures")
ggsave("prec dry north.tiff", dpi=600, compression = 'lzw')


ggplot(data=Data_North, aes(Classification_name, Distance_to_the_coast, fill=Classification_name))+ 
  theme_classic()+ 
  geom_violin(aes(fill=Classification_name))+ 
  geom_jitter(colour="grey23", position = position_jitter(width = 0.1), alpha = 0.5)+  
    scale_fill_manual(values = c("yellow", "green2", "magenta", "blue", "red3"))+
  geom_boxplot(width=0.15, fill="white") +
  ylab("Distance to the coast (km)") +
  theme(axis.title=element_text(size=19,face="bold"),
        axis.text.x = element_text(size = 16, color="black", face="bold"),
        axis.text.y = element_text(size = 16, color="black", face="bold"),
        legend.position = "none")+
  xlab("Chemotype")+
  ylim(0,1900)+
  theme(plot.margin = unit(c(1, 5, 1, 1), "cm"))

ggsave("dist north.tiff", dpi=600, compression = 'lzw')

#ggtitle("South")+ theme(plot.title = element_text(hjust=0.5))+

#legend:
ggplot(data=Data_North, aes(Classification_name, WC2_BIO14, fill=Classification_name))+ 
  theme_classic()+ 
  geom_violin(aes(fill=Classification_name))+ 
  geom_jitter(colour="grey23", position = position_jitter(width = 0.1), alpha = 0.5)+  
  scale_fill_manual(values = c("yellow", "green2", "magenta","blue", "red3"))+
  geom_boxplot(width=0.1, fill="white") +
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))+
  ylab("Precipitation of driest month (mm)") +
  theme(axis.title=element_text(size=19,face="bold"),
        axis.text.x = element_text(size = 16, color="black", face="bold"),
        axis.text.y = element_text(size = 16, color="black", face="bold"),
        legend.position = "right",
        legend.text = element_text(colour="black", size=12, face="bold"),
        legend.title = element_text(colour="black", size=14, face="bold"))+
  labs(fill = "Chemotype")+
  xlab("Chemotype")+
  ylim(0,125)+
  theme(plot.margin = unit(c(1, 5, 1, 1), "cm"))

ggsave("Legend.tiff", dpi=600, compression = 'lzw')

#For the paper - MAM violin plots:
ggplot(data=Data_2South, aes(MAM, Distance_to_the_coast, fill=MAM))+ 
  theme_classic()+ 
  geom_violin(aes(fill=MAM))+ 
  geom_jitter(colour="grey23", position = position_jitter(width = 0.1), alpha = 0.5)+  
  ggtitle("South")+ theme(plot.title = element_text(size=20, hjust=0.5, vjust = - 8, color="black", face="bold"))+
  scale_fill_manual(values = c("blue", "red"))+
  geom_boxplot(width=0.15, fill="white") +
  ylab("Distance to the coast (km)") +
    theme(axis.title=element_text(size=19,face="bold"),
        axis.text.x = element_text(size = 16, color="black", face="bold"),
        axis.text.y = element_text(size = 16, color="black", face="bold"),
        legend.position = "none")+
  xlab("C chain length")+
  ylim(0,1200)+
  theme(plot.margin = unit(c(1, 5, 1, 1), "cm"))

setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Enviroment/Violin plots carbon length")

ggsave("Distance South.tiff", dpi=600, compression = 'lzw')

t.test(Temp_warm~MAM, data=Data_2North)
t.test(Temp_cold~MAM, data=Data_2North)
t.test(Prec_wet~MAM, data=Data_2North)
t.test(Prec_dry~MAM, data=Data_2North)
t.test(Distance_to_the_coast~MAM, data=Data_2North)


t.test(Temp_warm~MAM, data=Data_2South)
t.test(Temp_cold~MAM, data=Data_2South)
t.test(Prec_wet~MAM, data=Data_2South)
t.test(Prec_dry~MAM, data=Data_2South)
t.test(Distance_to_the_coast~MAM, data=Data_2South)

#Leaving only Allyl and OHBut:
Data_1<-Data_1[!(Data_1$Classification_name=="3MSO"),]
Data_1<-Data_1[!(Data_1$Classification_name=="3OHP"),]
Data_1<-Data_1[!(Data_1$Classification_name=="4MSO"),]

Data_North<-Data_North[!(Data_North$Classification_name=="3MSO"),]
Data_North<-Data_North[!(Data_North$Classification_name=="3OHP"),]
Data_North<-Data_North[!(Data_North$Classification_name=="4MSO"),]

Data_South<-Data_South[!(Data_South$Classification_name=="3OHP"),]
Data_South<-Data_South[!(Data_South$Classification_name=="4MSO"),]

d <- lm(MAM_Status ~ WC2_BIO5+WC2_BIO6+ WC2_BIO13+ WC2_BIO14 + Distance_to_the_coast, data = Data_South)
summary(d)

d <- lm(Distance_to_the_coast ~ Classification_name, data = Data_South)
summary(d)

d <- lm(MAM_Status ~ Distance_to_the_coast, data = Data_North)
summary(d)


ggplot(data=Data_South, aes(Classification_name, WC2_BIO13, fill=Classification_name))+ 
  theme_classic()+ 
  geom_violin(aes(fill=Classification_name))+ 
  geom_jitter(colour="grey23", position = position_jitter(width = 0.1), alpha = 0.5)+  
  ggtitle("All Data")+ theme(plot.title = element_text(hjust=0.5))+
  scale_fill_manual(values = c("blue", "red3"))+
  geom_boxplot(width=0.1, fill="white") +
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))+
   ylab("Distance to the coast (km)") +
  xlab("Chemotype")+
  ylim(0,120)+
  theme(plot.margin = unit(c(6, 6, 1, 4), "cm"))



#ALL:
a1 <- manova(cbind(WC2_BIO13, WC2_BIO6, SRTM_elevation) ~ Classification_name, data = Data_1)
summary(a1)
summary.aov(a1)
cld(emmeans(a1, ~Classification_name))

a1 <- manova(cbind(WC2_BIO13, WC2_BIO6) ~ Classification_name, data = Data_1GIberian_Peninsula)
summary(a1)
summary.aov(a1)
cld(emmeans(a1, ~Classification_name))

a2 <- manova(cbind(WC2_BIO16, WC2_BIO6) ~ Classification_name, data = Data_1GIberian_Penincula)
summary(a2)
summary.aov(a2)
a3 <- manova(cbind(WC2_BIO19, WC2_BIO6) ~ Classification_name, data = Data_1GIberian_Penincula)
summary(a3)
summary.aov(a3)

a4 <- manova(cbind(WC2_BIO13, CHELSA_BIO11) ~ Classification_name, data = Data_1GIberian_Penincula)
summary(a4)
summary.aov(a4)
a5 <- manova(cbind(WC2_BIO16, CHELSA_BIO11) ~ Classification_name, data = Data_1GIberian_Penincula)
summary(a5)
summary.aov(a5)
cld(emmeans(a5, ~Classification_name))
a6 <- manova(cbind(WC2_BIO19, CHELSA_BIO11) ~ Classification_name, data = Data_1GIberian_Penincula)
summary(a6)
summary.aov(a6)

a7 <- manova(cbind(WC2_BIO13, CHELSA_BIO6) ~ Classification_name, data = Data_1GIberian_Penincula)
summary(a7)
summary.aov(a7)
cld(emmeans(a7, ~Classification_name))
a8 <- manova(cbind(WC2_BIO16, CHELSA_BIO6) ~ Classification_name, data = Data_1GIberian_Penincula)
summary(a8)
summary.aov(a8)
a9 <- manova(cbind(WC2_BIO19, CHELSA_BIO6) ~ Classification_name, data = Data_1GIberian_Penincula)
summary(a9)
summary.aov(a9)

a10 <- manova(cbind(WC2_BIO12, WC2_BIO7) ~ Classification_name, data = Data_1GIberian_Penincula)
summary(a10)
summary.aov(a10)
cld(emmeans(a10, ~Classification_name))
a10a <- manova(cbind(WC2_BIO12, WC2_BIO7) ~ MAM+AOP+GSOH+ MAM*AOP+ MAM*GSOH+ AOP*GSOH, data = Data_1GIberian_Penincula)
summary(a10a)
summary.aov(a10a)
a11 <- manova(cbind(WC2_BIO12, CHELSA_BIO7) ~ Classification_name, data = Data_1GIberian_Penincula)
summary(a11)
summary.aov(a11)
a12 <- manova(cbind(WC2_BIO12, CHELSA_Interann_Temp) ~ Classification_name, data = Data_1GIberian_Penincula)
summary(a12)
summary.aov(a12)
a13 <- manova(cbind(WC2_BIO12, CHELSA_BIO4) ~ Classification_name, data = Data_1GIberian_Penincula)
summary(a13)
summary.aov(a13)

a14 <- manova(cbind(WC2_BIO13, SRTM_elevation) ~ Classification_name, data = Data_1GIberian_Penincula)
summary(a14)
summary.aov(a14)
a15 <- manova(cbind(WC2_BIO16, SRTM_elevation) ~ Classification_name, data = Data_1GIberian_Penincula)
summary(a15)
summary.aov(a15)
a16 <- manova(cbind(WC2_BIO19, SRTM_elevation) ~ Classification_name, data = Data_1GIberian_Penincula)
summary(a16)
summary.aov(a16)
a17 <- manova(cbind(WC2_BIO12, SRTM_elevation) ~ Classification_name, data = Data_1GIberian_Penincula)
summary(a17)
summary.aov(a17)

a18 <- manova(cbind(WC2_BIO6, SRTM_elevation) ~ Classification_name, data = Data_1GIberian_Penincula)
summary(a18)
summary.aov(a18)
a19 <- manova(cbind(CHELSA_BIO11, SRTM_elevation) ~ Classification_name, data = Data_1GIberian_Penincula)
summary(a19)
summary.aov(a19)
a20 <- manova(cbind(CHELSA_BIO6, SRTM_elevation) ~ Classification_name, data = Data_1GIberian_Penincula)
summary(a20)
summary.aov(a20)


a21 <- manova(cbind(WC2_BIO7, WC2_BIO12, SRTM_elevation) ~ Classification_name, data = Data_1GIberian_Penincula)
summary(a21)
summary.aov(a21)
a21a <- manova(cbind(WC2_BIO7, WC2_BIO12, SRTM_elevation) ~ MAM+AOP+GSOH+ MAM*AOP+ MAM*GSOH+ AOP*GSOH, data = Data_1GIberian_Penincula)
summary(a21a)
summary.aov(a21a)


a22 <- manova(cbind(CHELSA_BIO11, WC2_BIO16, SRTM_elevation) ~ Classification_name, data = Data_1GIberian_Penincula)
summary(a22)
summary.aov(a22)
cld(emmeans(a22, ~Classification_name))
a22a <- manova(cbind(CHELSA_BIO11, WC2_BIO16, SRTM_elevation) ~ MAM+AOP+GSOH+ MAM*AOP+ MAM*GSOH+ AOP*GSOH, data = Data_1GIberian_Penincula)
summary(a22a)
summary.aov(a22a)


a23 <- manova(cbind(CHELSA_BIO13, WC2_BIO6, SRTM_elevation) ~ Classification_name, data = Data_1GIberian_Penincula)
summary(a23)
summary.aov(a23)
cld(emmeans(a23, ~Classification_name))
a23a <- manova(cbind(CHELSA_BIO13, WC2_BIO6, SRTM_elevation) ~  MAM+AOP+GSOH+ MAM*AOP+ MAM*GSOH+ AOP*GSOH, data = Data_1GIberian_Penincula)
summary(a23a)
summary.aov(a23a)
SRTM_elevation
CHELSA_BIO6

ggplot(data=Data_1GIberian_Peninsula, aes(Classification_name, SRTM_elevation, fill=Classification_name))+ 
  theme_minimal() + 
  geom_bar(aes(fill=Classification_name), stat = "summary", fun.y = "mean", position = position_dodge(), width=0.7)+ 
  geom_jitter(aes(colour = Classification_name), position = position_jitter(width = 0.1), alpha = 0.2) +  
  ggtitle("")+
  scale_fill_manual(values = c("green2", "magenta", "blue", "black", "red3"))+
  ylab("SRTM_elevation") +
  xlab("Chemotype") +
  theme(plot.margin = unit(c(1, 8, 1, 2), "cm"))

ggplot(data=Data_1GIberian_Penincula, aes(Classification_name, CHELSA_BIO11, fill=Classification_name))+ 
  theme_minimal() + 
  geom_bar(aes(fill=Classification_name), stat = "summary", fun.y = "mean", position = position_dodge(), width=0.7)+ 
  geom_jitter(aes(colour = Classification_name), position = position_jitter(width = 0.1), alpha = 0.2) +  
  ggtitle("")+
  scale_fill_manual(values = c("green2", "magenta", "blue", "black", "red3"))+
  ylab("CHELSA BIO11 = Mean temperature of coldest quarter") +
  xlab("Chemotype") +
  theme(plot.margin = unit(c(1, 8, 1, 2), "cm"))

ggplot(data=Data_1GIberian_Penincula, aes(Classification_name, WC2_BIO16, fill=Classification_name))+ 
  theme_minimal() + 
  geom_bar(aes(fill=Classification_name), stat = "summary", fun.y = "mean", position = position_dodge(), width=0.7)+ 
  geom_jitter(aes(colour = Classification_name), position = position_jitter(width = 0.1), alpha = 0.2) +  
  ggtitle("")+
  scale_fill_manual(values = c("green2", "magenta", "blue", "black", "red3"))+
  ylab("WorldClim BIO16 = Precipitation of wettest quarter") +
  xlab("Chemotype") +
  theme(plot.margin = unit(c(1, 8, 1, 2), "cm"))

ggplot(data=Data_1GIberian_Penincula, aes(Classification_name, WC2_BIO6, fill=Classification_name))+ 
  theme_minimal() + 
  geom_bar(aes(fill=Classification_name), stat = "summary", fun.y = "mean", position = position_dodge(), width=0.7)+ 
  geom_jitter(aes(colour = Classification_name), position = position_jitter(width = 0.1), alpha = 0.2) +  
  ggtitle("")+
  scale_fill_manual(values = c("green2", "magenta", "blue", "black", "red3"))+
  ylab("WorldClim BIO6 = Min temperature of coldest month 
") +
  xlab("Chemotype") +
  theme(plot.margin = unit(c(1, 8, 1, 2), "cm"))

ggplot(data=Data_1GIberian_Penincula, aes(Classification_name, WC2_BIO13, fill=Classification_name))+ 
  theme_minimal() + 
  geom_bar(aes(fill=Classification_name), stat = "summary", fun.y = "mean", position = position_dodge(), width=0.7)+ 
  geom_jitter(aes(colour = Classification_name), position = position_jitter(width = 0.1), alpha = 0.2) +  
  ggtitle("")+
  scale_fill_manual(values = c("green2", "magenta", "blue", "black", "red3"))+
  ylab("WorldClim BIO13 = Precipitation of wettest month") +
  xlab("Chemotype") +
  theme(plot.margin = unit(c(1, 8, 1, 2), "cm"))

#The rest of Europe:

a24 <- manova(cbind(CHELSA_BIO11, WC2_BIO16, SRTM_elevation) ~ Classification_name, data = Data_1GEurope)
summary(a24)
summary.aov(a24)
cld(emmeans(a24, ~Classification_name))
a24b <- manova(cbind(CHELSA_BIO11, WC2_BIO16) ~ Classification_name, data = Data_1GEurope)
cld(emmeans(a24b, ~Classification_name))
a24a <- manova(cbind(CHELSA_BIO11, WC2_BIO16, SRTM_elevation) ~ MAM+AOP+GSOH+ MAM*AOP+ MAM*GSOH+ AOP*GSOH, data = Data_1GEurope)
summary(a24a)
summary.aov(a24a)


a25 <- manova(cbind(CRU_Tmn_summer, CRU_Tmx_summer, CHELSA_BIO8 +CHELSA_BIO9) ~ Classification_name, data = Data_1GEurope)
summary(a25)
summary.aov(a25)
cld(emmeans(a25, ~Classification_name))
a25b <- manova(cbind(CRU_Tmn_summer, CRU_Tmx_summer) ~ Classification_name, data = Data_1)
cld(emmeans(a25b, ~Classification_name))
a25a <- manova(cbind(CRU_Tmn_summer, CRU_Tmx_summer) ~  MAM+AOP+GSOH+ MAM*AOP+ MAM*GSOH+ AOP*GSOH, data = Data_1GEurope)
summary(a25a)
summary.aov(a25a)


ggplot(data=Data_1GEurope, aes(Classification_name, WC2_BIO14, fill=Classification_name))+ 
  theme_minimal() + 
  geom_bar(aes(fill=Classification_name), stat = "summary", fun.y = "mean", position = position_dodge(), width=0.7)+ 
  geom_jitter(aes(colour = Classification_name), position = position_jitter(width = 0.1), alpha = 0.2) +  
  ggtitle("")+
  scale_fill_manual(values = c("orange", "yellow", "green2", "magenta", "skyblue", "blue", "black", "red3"))+
  ylab("SRTM_elevation") +
  xlab("Chemotype") +
  theme(plot.margin = unit(c(1, 8, 1, 2), "cm"), axis.text.x = element_text(angle = 45))

ggplot(data=Data_1GEurope, aes(Classification_name, CHELSA_BIO11, fill=Classification_name))+ 
  theme_minimal() + 
  geom_bar(aes(fill=Classification_name), stat = "summary", fun.y = "mean", position = position_dodge(), width=0.7)+ 
  geom_jitter(aes(colour = Classification_name), position = position_jitter(width = 0.1), alpha = 0.2) +  
  ggtitle("")+
  scale_fill_manual(values = c("orange", "yellow", "green2", "magenta", "skyblue", "blue", "black", "red3"))+
  ylab("CHELSA BIO11 = Mean temperature of coldest quarter") +
  xlab("Chemotype") +
  theme(plot.margin = unit(c(1, 8, 1, 2), "cm"), axis.text.x = element_text(angle = 45))

ggplot(data=Data_1GEurope, aes(Classification_name, WC2_BIO16, fill=Classification_name))+ 
  theme_minimal() + 
  geom_bar(aes(fill=Classification_name), stat = "summary", fun.y = "mean", position = position_dodge(), width=0.7)+ 
  geom_jitter(aes(colour = Classification_name), position = position_jitter(width = 0.1), alpha = 0.2) +  
  ggtitle("")+
  scale_fill_manual(values = c("orange", "yellow", "green2", "magenta", "skyblue", "blue", "black", "red3"))+
  ylab("WorldClim BIO16 = Precipitation of wettest quarter") +
  xlab("Chemotype") +
  theme(plot.margin = unit(c(1, 8, 1, 2), "cm"), axis.text.x = element_text(angle = 45))

ggplot(data=Data_1GEurope, aes(Classification_name, WC2_BIO6, fill=Classification_name))+ 
  theme_minimal() + 
  geom_bar(aes(fill=Classification_name), stat = "summary", fun.y = "mean", position = position_dodge(), width=0.7)+ 
  geom_jitter(aes(colour = Classification_name), position = position_jitter(width = 0.1), alpha = 0.2) +  
  ggtitle("")+
  scale_fill_manual(values = c("orange", "yellow", "green2", "magenta", "skyblue", "blue", "black", "red3"))+
  ylab("WorldClim BIO6 = Min temperature of coldest month 
") +
  xlab("Chemotype") +
  theme(plot.margin = unit(c(1, 8, 1, 2), "cm"), axis.text.x = element_text(angle = 45))

ggplot(data=Data_1GEurope, aes(Classification_name, SMN_Summer, fill=Classification_name))+ 
  theme_minimal() + 
  geom_bar(aes(fill=Classification_name), stat = "summary", fun.y = "mean", position = position_dodge(), width=0.7)+ 
  geom_jitter(aes(colour = Classification_name), position = position_jitter(width = 0.1), alpha = 0.2) +  
  ggtitle("")+
  scale_fill_manual(values = c("orange", "yellow", "green2", "magenta", "skyblue", "blue", "black", "red3"))+
  ylab("WorldClim BIO13 = Precipitation of wettest month") +
  xlab("Chemotype") +
  theme(plot.margin = unit(c(1, 8, 1, 2), "cm"), axis.text.x = element_text(angle = 45))


colnames(Data_1GEurope)
#Correlation plots:

ggplot(Data_1GIberian_Peninsula, aes(x=CHELSA_BIO8, y=WC2_BIO9, color=Classification_name)) + geom_point(size=2)+
scale_color_manual(values = c("green2", "magenta", "blue", "black", "red3"))+
  theme_bw()+
  ylab("WorldClim BIO16 = Precipitation of wettest quarter") +
  xlab("CHELSA BIO11 = Mean temperature of coldest quarter")

ggplot(Data_1GIberian_Penincula, aes(x=WC2_BIO6, y=WC2_BIO13, color=Classification_name)) + 
  geom_point(size=2)+
  scale_color_manual(values = c("green2", "magenta", "blue", "black", "red3"))+
  theme_bw()+
  ylab("WorldClim BIO13 = Precipitation of wettest month") +
  xlab("WorldClim BIO6 = Min temperature of coldest month")


ggplot(Data_1, aes(x=Distance_to_the_coast, y=SRTM_elevation, color=Classification_name)) + geom_point(size=2)+
  scale_color_manual(values = c("orange", "yellow", "green2", "magenta", "skyblue", "blue", "black", "red3"))+
  theme_bw()+
  ylab("WorldClim BIO16 = Precipitation of wettest quarter") +
  xlab("CHELSA BIO11 = Mean temperature of coldest quarter")

ggplot(Data_1GEurope, aes(x=WC2_BIO6, y=WC2_BIO13, color=Classification_name)) + 
  geom_point(size=2)+
  scale_color_manual(values = c("orange", "yellow", "green2", "magenta", "skyblue", "blue", "black", "red3"))+
  theme_bw()+
  ylab("WorldClim BIO13 = Precipitation of wettest month") +
  xlab("WorldClim BIO6 = Min temperature of coldest month")



#3D:
library(plotly)
plot_ly(Data_1GEurope, x = ~WC2_BIO6, y = ~WC2_BIO13, z = ~SRTM_elevation, color = ~ Classification_name, 
      size=10, alpha = 0.8)%>%
  add_markers() %>% 
  layout(scene = list(xaxis = list(title = 'WC2_BIO6'),
                      yaxis = list(title = 'WC2_BIO13'),
                      zaxis = list(title = 'SRTM_elevation'))) %>% 
  add_text(Data_1GEurope, x = ~WC2_BIO6, y = ~WC2_BIO13, z = ~SRTM_elevation)

plot_ly(Data_1GIberian_Penincula, x = ~WC2_BIO6, y = ~WC2_BIO13, z = ~SRTM_elevation, color = ~ Classification_name, 
        size=10, alpha = 0.8)%>%
  add_markers() %>% 
  layout(scene = list(xaxis = list(title = 'WC2_BIO6'),
                      yaxis = list(title = 'WC2_BIO13'),
                      zaxis = list(title = 'SRTM_elevation'))) %>% 
  add_text(Data_1GIberian_Penincula, x = ~WC2_BIO6, y = ~WC2_BIO13, z = ~SRTM_elevation)

plot_ly(Data_1GEurope, x = ~CHELSA_BIO11, y = ~WC2_BIO16, z = ~SRTM_elevation, color = ~ Classification_name, 
        size=10, alpha = 0.8)%>%
  add_markers() %>% 
  layout(scene = list(xaxis = list(title = 'CHELSA_BIO11'),
                      yaxis = list(title = 'WC2_BIO16'),
                      zaxis = list(title = 'SRTM_elevation'))) %>% 
  add_text(Data_1GEurope, x = ~CHELSA_BIO11, y = ~WC2_BIO16, z = ~SRTM_elevation)

plot_ly(Data_1GIberian_Penincula, x = ~CHELSA_BIO11, y = ~WC2_BIO16, z = ~SRTM_elevation, color = ~ Classification_name, 
        size=10, alpha = 0.8)%>%
  add_markers() %>% 
  layout(scene = list(xaxis = list(title = 'CHELSA_BIO11'),
                      yaxis = list(title = 'WC2_BIO16'),
                      zaxis = list(title = 'SRTM_elevation'))) %>% 
  add_text(Data_1GIberian_Penincula, x = ~CHELSA_BIO11, y = ~WC2_BIO16, z = ~SRTM_elevation)


#Maps:

print(mymap)+
  geom_point(data=Data_1, aes(x=Long, y=Lat, color=group), alpha = 7/10, size=2) +
  ggtitle('') +
  xlab('Longtitude') +
  ylab('Latitude') +
  theme(text = element_text(size=18)) +
  theme(axis.text=element_text(size=12,color='black', face="bold"),
        axis.title=element_text(size=14,color='black', face="bold")) +
  theme(plot.title = element_text(size = 26, vjust=1)) +
  theme(plot.title = element_text(size = 18,face="bold")) +
  theme(legend.key = element_rect(fill = "white")) +
  theme(legend.text = element_text(colour="black", size=12))+
  theme(legend.title = element_text(colour="black", size=12, face="bold"))+
  scale_color_manual(values = c("black", "orange", "grey", "gold", "brown", "dodgerblue2", "red", "green", "purple", "pink"))

setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/Manuscript/Figures")

ggsave("Map group.tiff", dpi=600, compression = 'lzw')


ggplot(Data_1, aes(x = group, fill = Classification_name)) +
  geom_bar(position = "fill") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, size=16), axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16), axis.text.y = element_text(size=16))+
  labs(fill = "Chemotype")+
  scale_fill_manual(values = c("yellow", "green2", "magenta", "skyblue", "blue", "black", "red3"))

ggsave("GSL by group.tiff", dpi=600, compression = 'lzw')

print(mymap)+
  geom_point(data=Data_1, aes(x=Long, y=Lat, color=AOP), alpha = 7/10, size=2) +
  ggtitle('') +
  xlab('Longtitude') +
  ylab('Latitude') +
  theme(text = element_text(size=18)) +
  theme(axis.text=element_text(size=12,color='black', face="bold"),
        axis.title=element_text(size=14,color='black', face="bold")) +
  theme(plot.title = element_text(size = 26, vjust=1)) +
  theme(plot.title = element_text(size = 18,face="bold")) +
  theme(legend.key = element_rect(fill = "white")) +
  theme(legend.text = element_text(colour="black", size=14, face="bold"))+
  theme(legend.title = element_text(colour="black", size=14, face="bold"))+
  theme(legend.position= c(0.075, 0.9))+
  scale_color_manual(values=c("magenta", "green", "gold"))

ggsave("Map AOP.tiff", dpi=600, compression = 'lzw')

print(mymap)+
  geom_point(data=Data_1, aes(x=Long, y=Lat, color=MAM), alpha = 7/10, size=2) +
  ggtitle('') +
  xlab('Longtitude') +
  ylab('Latitude') +
  theme(text = element_text(size=18)) +
  theme(axis.text=element_text(size=12,color='black', face="bold"),
        axis.title=element_text(size=14,color='black', face="bold")) +
  theme(plot.title = element_text(size = 26, vjust=1)) +
  theme(plot.title = element_text(size = 18,face="bold")) +
  theme(legend.key = element_rect(fill = "white")) +
  theme(legend.text = element_text(colour="black", size=14, face="bold"))+
  theme(legend.title = element_text(colour="black", size=14, face="bold"))+
  theme(legend.position= c(0.066, 0.92))+
  scale_color_manual(values=c("blue", "red"))

ggsave("Map Elong.tiff", dpi=600, compression = 'lzw')

print(mymap)+
  geom_point(data=Data_1, aes(x=Long, y=Lat, color=GSOH), alpha = 7/10, size=2) +
  ggtitle('') +
  xlab('Longtitude') +
  ylab('Latitude') +
  theme(text = element_text(size=18)) +
  theme(axis.text=element_text(size=12,color='black', face="bold"),
        axis.title=element_text(size=14,color='black', face="bold")) +
  theme(plot.title = element_text(size = 26, vjust=1)) +
  theme(plot.title = element_text(size = 18,face="bold")) +
  theme(legend.key = element_rect(fill = "white")) +
  theme(legend.text = element_text(colour="black", size=14, face="bold"))+
  theme(legend.title = element_text(colour="black", size=14, face="bold"))+
  theme(legend.position= c(0.066, 0.92))+
  scale_color_manual(values=c("black", "coral1"))

ggsave("Map GSOH.tiff", dpi=600, compression = 'lzw')
