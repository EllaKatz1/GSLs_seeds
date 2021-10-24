setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Pie charts")
Accessions_data <- read.table(file="Data_traits_short.csv", header=T, sep=",")
Enviromental_data <- read.table(file="shiny climatesd short.csv", header=T, sep=",")
Prec_group_Classification<- read.table(file="Prec_group_Classification.csv", header=T, sep=",")

colnames(Enviromental_data)
Groups <-Enviromental_data[,c(1,2)]

#Excluding the eastern accessions:
Data_1 <- merge(Accessions_data, Groups, by="CS")
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


Data_1<-Data_1[!(Data_1$Classification_name == "?"),]



#Plotiing on the map:
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
  geom_point(data=Data_1, aes(x=Long, y=Lat, color=GEO), alpha = 7/10, size=2) +
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
  scale_color_manual(values=c( "black","red"))

qplot(Data_1$C3ratio_emmeans, geom="histogram") +theme_bw()
ggplot(data=Data_1, aes(Data_1$C3ratio_emmeans)) + geom_histogram()+theme_bw()+
  ylab("# of accessions") +
  xlab("C3/C4 ratio")

lm_MAM <- aov(data=Data_1, C3ratio_emmeans ~ Long + Lat +Long*Lat)
anova(lm_MAM)

lm_Alk <- aov(data=Data_1, Alk_OH_ratio ~ Long + Lat +Long*Lat)
anova(lm_Alk)

lm_GSOH <- aov(data=Data_1, GSOH_emmeans ~ Long + Lat +Long*Lat)
anova(lm_GSOH)

print(mymap)+
  geom_point(data=Data_1, aes(x=Long, y=Lat, color=Classification_name), alpha = 7/10, size=2) +
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
  scale_color_manual(values = c("orange", "yellow", "green2", "magenta", "skyblue", "blue", "black", "red3"))


print(mymap)+
  geom_point(data=Data_1, aes(x=Long, y=Lat, color=Classification_name), alpha = 7/10, size=2) +
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
  scale_color_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a", "#e31a1c", "#fb9a99", "#33a02c", "#fdbf6f", "#ff7f00"))


print(mymap)+
  geom_point(data=Data_1, aes(x=Long, y=Lat, color=Classification_name), alpha = 7/10, size=2) +
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
  scale_color_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f", "#e5c494", "#b3b3b3"))

print(mymap)+
  geom_point(data=Data_1, aes(x=Long, y=Lat, color=Classification_name), alpha = 7/10, size=2) +
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
  scale_color_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ffff33", "#ff7f00", "#a65628", "#f781bf"))

colnames(Data_1)
names(Data_1)[31] <- "Genomic_Group"

print(mymap)+
  geom_point(data=Data_1, aes(x=Long, y=Lat, color=Genomic_Group), alpha = 7/10, size=2) +
  ggtitle('') +
  xlab('Longitude') +
  ylab('Latitude') +
  theme(text = element_text(size=16)) +
  theme(axis.text=element_text(size=16,color='black'),
        axis.title=element_text(size=18,color='black')) +
  theme(plot.title = element_text(size = 26, vjust=1)) +
  theme(plot.title = element_text(size = 18,face="bold")) +
  theme(legend.key = element_rect(fill = "white")) +
  theme(legend.text = element_text(colour="black", size=16))+
  theme(legend.title = element_text(colour="black", size=18, face="bold"))+
  scale_color_manual(values = c("black", "orange", "grey", "gold", "brown", "dodgerblue2", "red", "green", "purple", "pink"))

ggsave("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/Manuscript/Figures/More plots/Genomic_Group.jpeg", dpi = 600)

lm_lc <- aov(data=Data_1, lc ~ group + Classification_name)
anova(lm_lc)

Tlm_lc <- glht(lm_lc, linfct = mcp(group = "Tukey"))
cld(Tlm_lc)
Tlm_lc <- glht(lm_lc, linfct = mcp(Classification_name = "Tukey"))
cld(Tlm_lc)
tukey.test <- TukeyHSD(lm_lc)
tukey.test

p<- ggplot(data = Data_1, aes(x=group, y=Classification_name, color=Classification_name))
p + geom_jitter(alpha=0.3) 

install.packages("MASS")
library(MASS)      
install.packages("vcd")
library(vcd)
tbl = table(Data_1$Classification_name, Data_1$group) 
tbl
chisq.test(tbl)
assocstats(tbl)


#Separating the groups:

Data_1$Group_1 <- paste("G", Data_1$group, sep="")
Data_1$Group_1 <- as.factor(Data_1$Group_1)
Data_1_Group <- split(Data_1, Data_1$Group_1)
list2env(Data_1_Group, envir=.GlobalEnv)
rm(Data_1_Group)

Data_1GAsia <- droplevels.data.frame(GAsia)
Data_1GAdmixed <- droplevels.data.frame(GAdmixed)
Data_1GCentralEurope <- droplevels.data.frame(GCentralEurope)
Data_1GGermany <- droplevels.data.frame(GGermany)
Data_1Gitaly_balkan_caucasus <- droplevels.data.frame(Gitaly_balkan_caucasus)
Data_1GNorthSweden <- droplevels.data.frame(GNorthSweden)
Data_1Grelict <- droplevels.data.frame(Grelict)
Data_1GSouthSweden <- droplevels.data.frame(GSouthSweden)
Data_1Gspain <- droplevels.data.frame(Gspain)
Data_1GWesternEurope <- droplevels.data.frame(GWesternEurope)

Data_sc_lc_BZO <- read.table(file="Data_sc_lc_BZO.csv", header=T, sep=",")
a1 <- aov(data=Data_1, lc ~ group)
anova(a1)
Ta1 <- glht(a1, linfct = mcp(group = "Tukey"))
cld(Ta1)

a2 <- aov(data=Data_1, sc ~ group)
anova(a2)
Ta2 <- glht(a2, linfct = mcp(group = "Tukey"))
cld(Ta2)

a3 <- aov(data=Data_1, BZO_ratio ~ group)
anova(a3)
Ta3 <- glht(a3, linfct = mcp(group = "Tukey"))
cld(Ta3)

a4 <- aov(data=Data_1, Total_GSL ~ group)
anova(a4)
Ta4 <- glht(a4, linfct = mcp(group = "Tukey"))
cld(Ta4)

ggplot(Data_sc_lc_BZO)+
   geom_bar(mapping = aes(x = Trait, y = Amount, fill= Group), 
  stat = "summary", fun.y = "mean", position = position_dodge())+
     theme_bw()+
  scale_fill_manual(values = c("black", "orange", "grey", "gold", "brown", "dodgerblue2", "red", "green", "purple", "pink"))+
  theme(plot.margin = unit(c(1, 3, 1, 2), "cm")) +
   ggtitle("") +
  xlab("Trait") +
  ylab("Amount")

ggplot(data=Data_sc_lc_BZO, aes(Trait, Amount, fill=Group))+ 
  theme_minimal() + 
  geom_bar(aes(fill=Group), stat = "summary", fun.y = "mean", position = position_dodge(), width=0.8)+ 
  geom_jitter(aes(colour = Group), position = position_jitter(width = 0.5), alpha = 0.2) +  
  ggtitle("")+
  scale_fill_manual(values = c("black", "orange", "grey", "gold", "brown", "dodgerblue2", "red", "green", "purple", "pink"))+
  ylab("Amount") +
  xlab("Trait") +
  theme(plot.margin = unit(c(1, 3, 1, 2), "cm"))



#Spain:

ggplot(Data_1Gspain, aes(x=factor(1), fill=Classification_name))+
  geom_bar(width = 1)+
   coord_polar("y")+ theme_void()+
  labs(x = NULL, y = NULL, fill = NULL, title = "Chemotypes - Spain")+
  scale_fill_manual(values = c("green2", "magenta", "blue", "black", "red3"))+
  theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5, color = "black"))+
  theme(plot.margin = unit(c(1, 9, 8, 5), "cm"))

colnames(Data_1Gspain)
Data_Spain <- Data_1Gspain[,c(3,10,11)]
write.csv(Data_Spain, file = "Data_Spain.csv")
Data_Spain_1 <- read.table(file="Data_Spain.csv", header=T, sep=",")

ggplot(data=Data_Spain_1, aes(Trait, Amount, fill=Trait)) +
    theme_minimal() + 
  geom_errorbar(data=Data_Spain_1,  aes(x= Trait, ymin = Amount, ymax = Amount+(Amount/sqrt(length(Amount)))), position=position_dodge(.4), width = 0.25, color= "black")+
    geom_bar(aes(fill=Trait), stat = "identity", position = position_dodge(), width=0.4)+ 
    ggtitle("Different traits - Spain")+
    theme(axis.text.x=element_text(angle=45,hjust=1))+
  scale_fill_manual(values = c("orange", "gold", "red2"))+
  ylab("GSLs amount
       (ratio to Total GSLs)") +
  theme(plot.margin = unit(c(1, 9, 8, 5), "cm"))
  
ggplot(data=Data_Spain_1, aes(Trait, Amount, fill=Trait)) +
  theme_minimal() + 
   geom_bar(aes(fill=Trait), stat = "summary", fun.y = "mean", position = position_dodge(), width=0.4)+ 
  geom_jitter(color="black", width = 0.1, alpha=0.2)+  
  ggtitle("Different traits - Spain")+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  scale_fill_manual(values = c("orange", "gold", "red2"))+
  ylab("GSLs amount
       (ratio to Total GSLs)") +
  theme(plot.margin = unit(c(1, 9, 8, 5), "cm"))


#Asia:

ggplot(Data_1GAsia, aes(x=factor(1), fill=Classification_name))+
  geom_bar(width = 1)+
  coord_polar("y")+ theme_void()+
  labs(x = NULL, y = NULL, fill = NULL, title = "Chemotypes - Asia")+
  scale_fill_manual(values = c("green2", "blue",  "black", "red2"))+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black"))+
  theme(plot.margin = unit(c(1, 9, 8, 5), "cm"))

colnames(Data_1Gspain)
Data_Asia <- Data_1GAsia[,c(3,10,11)]
write.csv(Data_Asia, file = "Data_Asia.csv")
Data_Asia_1 <- read.table(file="Data_Asia.csv", header=T, sep=",")

ggplot(data=Data_Asia_1, aes(Trait, Amount, fill=Trait)) +
  theme_minimal() + 
  geom_errorbar(data=Data_Asia_1,  aes(x= Trait, ymin = Amount, ymax = Amount+(Amount/sqrt(length(Amount)))), position=position_dodge(.4), width = 0.25, color= "black")+
  geom_bar(aes(fill=Trait), stat = "identity", position = position_dodge(), width=0.4)+ 
  ggtitle("Different traits - Asia")+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  scale_fill_manual(values = c("orange", "gold", "red2"))+
  ylab("GSLs amount
       (ratio to Total GSLs)") +
  theme(plot.margin = unit(c(1, 9, 8, 5), "cm"))

ggplot(data=Data_Asia_1, aes(Trait, Amount, fill=Trait)) +
  theme_minimal() + 
  geom_bar(aes(fill=Trait), stat = "summary", fun.y = "mean", position = position_dodge(), width=0.4)+ 
  geom_jitter(color="black", width = 0.1, alpha=0.2)+  
  ggtitle("Different traits - Asia")+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  scale_fill_manual(values = c("orange", "gold", "red2"))+
  ylab("GSLs amount
       (ratio to Total GSLs)") +
  theme(plot.margin = unit(c(1, 9, 8, 5), "cm"))


#Admixed:

ggplot(Data_1GAdmixed, aes(x=factor(1), fill=Classification_name))+
  geom_bar(width = 1)+
  coord_polar("y")+ theme_void()+
  labs(x = NULL, y = NULL, fill = NULL, title = "Chemotypes - Admixed")+
  scale_fill_manual(values = c("orange", "yellow", "green2", "magenta", "skyblue", "blue",  "red3"))+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black"))+
  theme(plot.margin = unit(c(1, 9, 8, 5), "cm"))

colnames(Data_1GAdmixed)
Data_Admixed <- Data_1GAdmixed[,c(3,10,11)]
write.csv(Data_Admixed, file = "Data_Admixed.csv")
Data_Admixed_1 <- read.table(file="Data_Admixed.csv", header=T, sep=",")

ggplot(data=Data_Admixed_1, aes(Trait, Amount, fill=Trait)) +
  theme_minimal() + 
  geom_errorbar(data=Data_Admixed_1,  aes(x= Trait, ymin = Amount, ymax = Amount+(Amount/sqrt(length(Amount)))), position=position_dodge(.4), width = 0.25, color= "black")+
  geom_bar(aes(fill=Trait), stat = "identity", position = position_dodge(), width=0.4)+ 
  ggtitle("Different traits - Admixed")+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  scale_fill_manual(values = c("orange", "gold", "red2"))+
  ylab("GSLs amount
       (ratio to Total GSLs)") +
  theme(plot.margin = unit(c(1, 9, 8, 5), "cm"))

ggplot(data=Data_Admixed_1, aes(Trait, Amount, fill=Trait)) +
  theme_minimal() + 
  geom_bar(aes(fill=Trait), stat = "summary", fun.y = "mean", position = position_dodge(), width=0.4)+ 
  geom_jitter(color="black", width = 0.1, alpha=0.2)+  
  ggtitle("Different traits - Admixed")+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  scale_fill_manual(values = c("orange", "gold", "red2"))+
  ylab("GSLs amount
       (ratio to Total GSLs)") +
  theme(plot.margin = unit(c(1, 9, 8, 5), "cm"))


#CentralEurope:

ggplot(Data_1GCentralEurope, aes(x=factor(1), fill=Classification_name))+
  geom_bar(width = 1)+
  coord_polar("y")+ theme_void()+
  labs(x = NULL, y = NULL, fill = NULL, title = "Chemotypes - CentralEurope")+
  scale_fill_manual(values = c("orange", "yellow", "green2", "magenta", "blue",  "red3"))+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black"))+
  theme(plot.margin = unit(c(1, 9, 8, 5), "cm"))

colnames(Data_1GCentralEurope)
Data_CentralEurope <- Data_1GCentralEurope[,c(3,10,11)]
write.csv(Data_CentralEurope, file = "Data_CentralEurope.csv")
Data_CentralEurope_1 <- read.table(file="Data_CentralEurope.csv", header=T, sep=",")

ggplot(data=Data_CentralEurope_1, aes(Trait, Amount, fill=Trait)) +
  theme_minimal() + 
  geom_errorbar(data=Data_Admixed_1,  aes(x= Trait, ymin = Amount, ymax = Amount+(Amount/sqrt(length(Amount)))), position=position_dodge(.4), width = 0.25, color= "black")+
  geom_bar(aes(fill=Trait), stat = "identity", position = position_dodge(), width=0.4)+ 
  ggtitle("Different traits - CentralEurope")+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  scale_fill_manual(values = c("orange", "gold", "red2"))+
  ylab("GSLs amount
       (ratio to Total GSLs)") +
  theme(plot.margin = unit(c(1, 9, 8, 5), "cm"))

ggplot(data=Data_CentralEurope_1, aes(Trait, Amount, fill=Trait)) +
  theme_minimal() + 
  geom_bar(aes(fill=Trait), stat = "summary", fun.y = "mean", position = position_dodge(), width=0.4)+ 
  geom_jitter(color="black", width = 0.1, alpha=0.2)+  
  ggtitle("Different traits - CentralEurope")+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  scale_fill_manual(values = c("orange", "gold", "red2"))+
  ylab("GSLs amount
       (ratio to Total GSLs)") +
  theme(plot.margin = unit(c(1, 9, 8, 5), "cm"))

#WesternEurope:

ggplot(Data_1GWesternEurope, aes(x=factor(1), fill=Classification_name))+
  geom_bar(width = 1)+
  coord_polar("y")+ theme_void()+
  labs(x = NULL, y = NULL, fill = NULL, title = "Chemotypes - WesternEurope")+
  scale_fill_manual(values = c("orange", "yellow", "green2", "magenta", "blue",  "red3"))+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black"))+
  theme(plot.margin = unit(c(1, 9, 8, 5), "cm"))

colnames(Data_1GWesternEurope)
Data_WesternEurope <- Data_1GWesternEurope[,c(3,10,11)]
write.csv(Data_WesternEurope, file = "Data_WesternEurope.csv")
Data_WesternEurope_1 <- read.table(file="Data_WesternEurope.csv", header=T, sep=",")

ggplot(data=Data_WesternEurope_1, aes(Trait, Amount, fill=Trait)) +
  theme_minimal() + 
  geom_errorbar(data=Data_Admixed_1,  aes(x= Trait, ymin = Amount, ymax = Amount+(Amount/sqrt(length(Amount)))), position=position_dodge(.4), width = 0.25, color= "black")+
  geom_bar(aes(fill=Trait), stat = "identity", position = position_dodge(), width=0.4)+ 
  ggtitle("Different traits - WesternEurope")+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  scale_fill_manual(values = c("orange", "gold", "red2"))+
  ylab("GSLs amount
       (ratio to Total GSLs)") +
  theme(plot.margin = unit(c(1, 9, 8, 5), "cm"))

ggplot(data=Data_WesternEurope_1, aes(Trait, Amount, fill=Trait)) +
  theme_minimal() + 
  geom_bar(aes(fill=Trait), stat = "summary", fun.y = "mean", position = position_dodge(), width=0.4)+ 
  geom_jitter(color="black", width = 0.1, alpha=0.2)+  
  ggtitle("Different traits - WesternEurope")+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  scale_fill_manual(values = c("orange", "gold", "red2"))+
  ylab("GSLs amount
       (ratio to Total GSLs)") +
  theme(plot.margin = unit(c(1, 9, 8, 5), "cm"))

#CentralGermany:

ggplot(Data_1GGermany, aes(x=factor(1), fill=Classification_name))+
  geom_bar(width = 1)+
  coord_polar("y")+ theme_void()+
  labs(x = NULL, y = NULL, fill = NULL, title = "Chemotypes - Germany")+
  scale_fill_manual(values = c("orange", "yellow", "green2", "magenta", "blue",  "red3"))+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black"))+
  theme(plot.margin = unit(c(1, 9, 8, 5), "cm"))

colnames(Data_1GGermany)
Data_Germany <- Data_1GGermany[,c(3,10,11)]
write.csv(Data_Germany, file = "Data_Germany.csv")
Data_Germany_1 <- read.table(file="Data_Germany.csv", header=T, sep=",")

ggplot(data=Data_Germany_1, aes(Trait, Amount, fill=Trait)) +
  theme_minimal() + 
  geom_errorbar(data=Data_Admixed_1,  aes(x= Trait, ymin = Amount, ymax = Amount+(Amount/sqrt(length(Amount)))), position=position_dodge(.4), width = 0.25, color= "black")+
  geom_bar(aes(fill=Trait), stat = "identity", position = position_dodge(), width=0.4)+ 
  ggtitle("Different traits - Germany")+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  scale_fill_manual(values = c("orange", "gold", "red2"))+
  ylab("GSLs amount
       (ratio to Total GSLs)") +
  theme(plot.margin = unit(c(1, 9, 8, 5), "cm"))

ggplot(data=Data_Germany_1, aes(Trait, Amount, fill=Trait)) +
  theme_minimal() + 
  geom_bar(aes(fill=Trait), stat = "summary", fun.y = "mean", position = position_dodge(), width=0.4)+ 
  geom_jitter(color="black", width = 0.1, alpha=0.2)+  
  ggtitle("Different traits - Germany")+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  scale_fill_manual(values = c("orange", "gold", "red2"))+
  ylab("GSLs amount
       (ratio to Total GSLs)") +
  theme(plot.margin = unit(c(1, 9, 8, 5), "cm"))

#NorthSweden:

ggplot(Data_1GNorthSweden, aes(x=factor(1), fill=Classification_name))+
  geom_bar(width = 1)+
  coord_polar("y")+ theme_void()+
  labs(x = NULL, y = NULL, fill = NULL, title = "Chemotypes - NorthSweden")+
  scale_fill_manual(values = c("blue"))+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black"))+
  theme(plot.margin = unit(c(1, 9, 8, 5), "cm"))

colnames(Data_1GNorthSweden)
Data_NorthSweden <- Data_1GNorthSweden[,c(3,10,11)]
write.csv(Data_NorthSweden, file = "Data_NorthSweden.csv")
Data_NorthSweden_1 <- read.table(file="Data_NorthSweden.csv", header=T, sep=",")

ggplot(data=Data_NorthSweden_1, aes(Trait, Amount, fill=Trait)) +
  theme_minimal() + 
  geom_errorbar(data=Data_Admixed_1,  aes(x= Trait, ymin = Amount, ymax = Amount+(Amount/sqrt(length(Amount)))), position=position_dodge(.4), width = 0.25, color= "black")+
  geom_bar(aes(fill=Trait), stat = "identity", position = position_dodge(), width=0.4)+ 
  ggtitle("Different traits - NorthSweden")+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  scale_fill_manual(values = c("orange", "gold", "red2"))+
  ylab("GSLs amount
       (ratio to Total GSLs)") +
  theme(plot.margin = unit(c(1, 9, 8, 5), "cm"))

ggplot(data=Data_NorthSweden_1, aes(Trait, Amount, fill=Trait)) +
  theme_minimal() + 
  geom_bar(aes(fill=Trait), stat = "summary", fun.y = "mean", position = position_dodge(), width=0.4)+ 
  geom_jitter(color="black", width = 0.1, alpha=0.2)+  
  ggtitle("Different traits - NorthSweden")+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  scale_fill_manual(values = c("orange", "gold", "red2"))+
  ylab("GSLs amount
       (ratio to Total GSLs)") +
  theme(plot.margin = unit(c(1, 9, 8, 5), "cm"))


#SouthSweden:

ggplot(Data_1GSouthSweden, aes(x=factor(1), fill=Classification_name))+
  geom_bar(width = 1)+
  coord_polar("y")+ theme_void()+
  labs(x = NULL, y = NULL, fill = NULL, title = "Chemotypes - SouthSweden")+
  scale_fill_manual(values = c("orange", "yellow", "green2", "magenta", "blue", "black", "red3"))+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black"))+
  theme(plot.margin = unit(c(1, 9, 8, 5), "cm"))

colnames(Data_1GSouthSweden)
Data_SouthSweden <- Data_1GSouthSweden[,c(3,10,11)]
write.csv(Data_SouthSweden, file = "Data_SouthSweden.csv")
Data_SouthSweden_1 <- read.table(file="Data_SouthSweden.csv", header=T, sep=",")

ggplot(data=Data_SouthSweden_1, aes(Trait, Amount, fill=Trait)) +
  theme_minimal() + 
  geom_errorbar(data=Data_Admixed_1,  aes(x= Trait, ymin = Amount, ymax = Amount+(Amount/sqrt(length(Amount)))), position=position_dodge(.4), width = 0.25, color= "black")+
  geom_bar(aes(fill=Trait), stat = "identity", position = position_dodge(), width=0.4)+ 
  ggtitle("Different traits - SouthSweden")+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  scale_fill_manual(values = c("orange", "gold", "red2"))+
  ylab("GSLs amount
       (ratio to Total GSLs)") +
  theme(plot.margin = unit(c(1, 9, 8, 5), "cm"))

ggplot(data=Data_SouthSweden_1, aes(Trait, Amount, fill=Trait)) +
  theme_minimal() + 
  geom_bar(aes(fill=Trait), stat = "summary", fun.y = "mean", position = position_dodge(), width=0.4)+ 
  geom_jitter(color="black", width = 0.1, alpha=0.2)+  
  ggtitle("Different traits - SouthSweden")+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  scale_fill_manual(values = c("orange", "gold", "red2"))+
  ylab("GSLs amount
       (ratio to Total GSLs)") +
  theme(plot.margin = unit(c(1, 9, 8, 5), "cm"))

#italy_balkan_caucasus:

ggplot(Data_1Gitaly_balkan_caucasus, aes(x=factor(1), fill=Classification_name))+
  geom_bar(width = 1)+
  coord_polar("y")+ theme_void()+
  labs(x = NULL, y = NULL, fill = NULL, title = "Chemotypes - italy_balkan_caucasus")+
  scale_fill_manual(values = c("orange", "yellow", "green2", "magenta", "blue", "red3"))+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black"))+
  theme(plot.margin = unit(c(1, 9, 8, 5), "cm"))

colnames(Data_1Gitaly_balkan_caucasus)
Data_italy_balkan_caucasus <- Data_1Gitaly_balkan_caucasus[,c(3,10,11)]
write.csv(Data_italy_balkan_caucasus, file = "Data_italy_balkan_caucasus.csv")
Data_italy_balkan_caucasus_1 <- read.table(file="Data_italy_balkan_caucasus.csv", header=T, sep=",")

ggplot(data=Data_italy_balkan_caucasus_1, aes(Trait, Amount, fill=Trait)) +
  theme_minimal() + 
  geom_errorbar(data=Data_Admixed_1,  aes(x= Trait, ymin = Amount, ymax = Amount+(Amount/sqrt(length(Amount)))), position=position_dodge(.4), width = 0.25, color= "black")+
  geom_bar(aes(fill=Trait), stat = "identity", position = position_dodge(), width=0.4)+ 
  ggtitle("Different traits - italy_balkan_caucasus")+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  scale_fill_manual(values = c("orange", "gold", "red2"))+
  ylab("GSLs amount
       (ratio to Total GSLs)") +
  theme(plot.margin = unit(c(1, 9, 8, 5), "cm"))

ggplot(data=Data_italy_balkan_caucasus_1, aes(Trait, Amount, fill=Trait)) +
  theme_minimal() + 
  geom_bar(aes(fill=Trait), stat = "summary", fun.y = "mean", position = position_dodge(), width=0.4)+ 
  geom_jitter(color="black", width = 0.1, alpha=0.2)+  
  ggtitle("Different traits - italy_balkan_caucasus")+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  scale_fill_manual(values = c("orange", "gold", "red2"))+
  ylab("GSLs amount
       (ratio to Total GSLs)") +
  theme(plot.margin = unit(c(1, 9, 8, 5), "cm"))

#Relict:

ggplot(Data_1Grelict, aes(x=factor(1), fill=Classification_name))+
  geom_bar(width = 1)+
  coord_polar("y")+ theme_void()+
  labs(x = NULL, y = NULL, fill = NULL, title = "Chemotypes - Relict")+
  scale_fill_manual(values = c("orange", "blue", "black", "red3"))+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black"))+
  theme(plot.margin = unit(c(1, 9, 8, 5), "cm"))

colnames(Data_1Grelict)
Data_relict <- Data_1Grelict[,c(3,10,11)]
write.csv(Data_relict, file = "Data_relict.csv")
Data_relict_1 <- read.table(file="Data_relict.csv", header=T, sep=",")

ggplot(data=Data_relict_1, aes(Trait, Amount, fill=Trait)) +
  theme_minimal() + 
  geom_errorbar(data=Data_Admixed_1,  aes(x= Trait, ymin = Amount, ymax = Amount+(Amount/sqrt(length(Amount)))), position=position_dodge(.4), width = 0.25, color= "black")+
  geom_bar(aes(fill=Trait), stat = "identity", position = position_dodge(), width=0.4)+ 
  ggtitle("Different traits - Relict")+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  scale_fill_manual(values = c("orange", "gold", "red2"))+
  ylab("GSLs amount
       (ratio to Total GSLs)") +
  theme(plot.margin = unit(c(1, 9, 8, 5), "cm"))

ggplot(data=Prec_group_Classification, aes(Trait, Amount, fill=Trait)) +
  theme_minimal() + 
  geom_bar(aes(fill=Trait), stat = "summary", fun.y = "mean", position = position_dodge(), width=0.4)+ 
  geom_jitter(color="black", width = 0.1, alpha=0.2)+  
  ggtitle("Different traits - Relict")+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  scale_fill_manual(values = c("orange", "gold", "red2"))+
  ylab("GSLs amount
       (ratio to Total GSLs)") +
  theme(plot.margin = unit(c(1, 9, 8, 5), "cm"))

#Group by Classification:

ggplot(data=Prec_group_Classification, aes(group, Prec, fill=Classification_name))+
  theme_minimal() + 
    geom_bar(aes(fill=Classification_name), stat = "identity", position = position_dodge())+ 
  ggtitle("")+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  scale_fill_manual(values = c("black", "red3", "orange", "yellow", "green2", "magenta", "skyblue", "blue"))+
  ylab("%")
  
g <- ggplot(Data_1, aes(Genomic_Group))
g + geom_bar(aes(fill=Classification_name), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  theme_bw()+
  scale_fill_manual(values = c("orange", "yellow", "green2", "magenta", "skyblue", "blue", "black", "red3"))+
theme(axis.text.x=element_text(angle=45,hjust=1))


tbl = table(Data_1$group, Data_1$Classification_name) 
tbl 
chisq.test(tbl) 

#Or:
ggplot(Data_1, aes(x = Genomic_Group, fill = Classification_name)) +
  geom_bar(position = "fill") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, size = 16, hjust=1),
        axis.text.y = element_text(size = 16),
        legend.text = element_text(size=16),
        legend.title = element_text(size=18),
        axis.title=element_text(size=18))+
  xlab("Genomic Group")+
  ylab("Prcentage")+
  scale_fill_manual(name = "Chemotype", values = c("yellow", "green2", "magenta", "skyblue", "blue", "black", "red3"))

ggsave("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/Manuscript/Figures/More plots/Genomic Group Prec.jpeg", dpi = 600)


theme(plot.title = element_text(size = 14, face = "bold"),
      axis.title=element_text(size=14,face="bold"),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      legend.text = element_text(size=14),
      legend.title = element_text(size=16),
      plot.margin = unit(c(0, 4, 0, 0), "cm"))


#Grouping by geography:
Data_1$GEO <- NA
Data_1$GEO[which(Data_1$Country == "ESP")] <- "Iberian_Penincula"
Data_1$GEO[which(Data_1$Country == "POR")] <- "Iberian_Penincula"
Data_1$GEO[is.na(Data_1$GEO)] <- "Europe"

print(mymap)+
  geom_point(data=Data_1, aes(x=Long, y=Lat, color=GEO), alpha = 7/10, size=2) +
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
  scale_color_manual(values=c( "black","red"))

#Separating the GEO:

Data_1$GEO_1 <- paste("G", Data_1$GEO, sep="")
Data_1$GEO_1 <- as.factor(Data_1$GEO_1)
Data_1_GEO <- split(Data_1, Data_1$GEO_1)
list2env(Data_1_GEO, envir=.GlobalEnv)
rm(Data_1_GEO)

Data_1GIberian_Penincula <- droplevels.data.frame(GIberian_Penincula)
Data_1GEurope <- droplevels.data.frame(GEurope)

ggplot(Data_1GIberian_Penincula, aes(x=factor(1), fill=Classification_name))+
  geom_bar(width = 1)+
  coord_polar("y")+ theme_void()+
    labs(x = NULL, y = NULL, fill = NULL, title = "Chemotypes - Iberian Peninsula")+
  scale_fill_manual(values = c("green2", "magenta", "blue", "black", "red3"))+
  geom_text(aes(x = 1.2, y = Classification_name, label = labels)) +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black"))+
  theme(plot.margin = unit(c(1, 9, 8, 5), "cm"))


ggplot(Data_1GEurope, aes(x=factor(1), fill=Classification_name))+
  geom_bar(width = 1)+
  coord_polar("y")+ theme_void()+
  labs(x = NULL, y = NULL, fill = NULL, title = "Chemotypes - Europe")+
  scale_fill_manual(values = c("orange", "yellow", "green2", "magenta", "skyblue", "blue", "black", "red3"))+
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black"))+
  theme(plot.margin = unit(c(1, 9, 8, 5), "cm"))

#Chi test for chemotypes
count(Data_1GIberian_Penincula$Classification_name)
table(Data_1GIberian_Penincula$Classification_name)
table2 <- table(Data_1GIberian_Penincula$Classification_name)
prop.table(table2)

count(Data_1GEurope$Classification_name)
table(Data_1GEurope$Classification_name)
table2 <- table(Data_1GEurope$Classification_name)
prop.table(table2)

Prec_data <- read.table(file="Chemotypes Prec.csv", header=T, sep=",")
colnames(Prec_data)
Prec_data_1 <- Prec_data[,c(3,5)]
dt <- as.table(as.matrix(Prec_data_1))
library("graphics")
mosaicplot(dt, shade = TRUE, las=2,
           main = "Chemotypes %")
balloonplot(t(dt), main ="housetasks", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
chisq <- chisq.test(Prec_data_1)
chisq

#Chi test for groups:
count(Data_1GIberian_Penincula$group)
table(Data_1GIberian_Penincula$group)
table2 <- table(Data_1GIberian_Penincula$group)
prop.table(table2)

count(Data_1GEurope$group)
table(Data_1GEurope$group)
table2 <- table(Data_1GEurope$group)
prop.table(table2)

Perc_group <- read.table(file="groupes Perc.csv", header=T, sep=",")
colnames(Perc_group)
Perc_group_1 <- Perc_group[,c(4,7)]
dt <- as.table(as.matrix(Perc_group_1))
library("graphics")
mosaicplot(dt, shade = TRUE, las=2,
           main = "group %")
balloonplot(t(dt), main ="housetasks", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)
chisq <- chisq.test(Perc_group_1)
chisq






treemap(Data_1GEurope, index="Classification_name", type="value", vSize ="Total_GSL")
install.packages("treemap")
library(treemap)

install.packages("gplots")
library("gplots")
balloonplot(t(tbl), main ="Classification_name", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

install.packages("rcompanion")
library(rcompanion)

pairwiseNominalIndependence(tbl,
                            fisher = FALSE,
                            gtest  = FALSE,
                            chisq  = TRUE,
                            method = "fdr")

tab2 <- chisq.test(tbl)
tab2$stdres


tbIberian <- tbl[-c(2:6,8),]
tbIberian
chisq.test(tbIberian)


#Geting the freq of Classification by groups:
freq(Data_1GGermany$Classification_name)
Prec_group_Classification <- read.table(file="Prec_group_Classification.csv", header=T, sep=",")
Prec_group_Classification[is.na(Prec_group_Classification)] <- 0

lm_Prec <- aov(data=Prec_group_Classification, Prec~ Classification_name+ group )
anova(lm_Prec)

Tlm_Prec <- glht(lm_Prec, linfct = mcp(group = "Tukey"))
cld(Tlm_Prec)

tukey.test <- TukeyHSD(lm_Prec)
tukey.test
Prec_Classification <- read.table(file="Prec_Classification.csv", header=T, sep=",")
Prec_Classification[is.na(Prec_Classification)] <- 0

lm_Prec <- aov(data=Prec_Classification, Prec~ Classification_name+ group )
anova(lm_Prec)

Tlm_Prec <- glht(lm_Prec, linfct = mcp(group = "Tukey"))
cld(Tlm_Prec)

tukey.test <- TukeyHSD(lm_Prec)
tukey.test

