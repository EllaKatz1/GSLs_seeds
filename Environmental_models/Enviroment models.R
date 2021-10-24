setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Enviroment")
Accessions_data <- read.table(file="Data_traits_short.csv", header=T, sep=",")
Enviromental_data <- read.table(file="shiny climatesd short.csv", header=T, sep=",")

colnames(Data_1)
Data_1 <- merge(Accessions_data, Enviromental_data, by="CS")


lm_LAI_Spring <- lm(data=Data_1, LAI_Spring ~ MAM + AOP + GSOH +MAM*AOP+ MAM*GSOH+ AOP*GSOH +Classification_name)
lm_LAI_Spring <- lm(data=Data_1, LAI_Spring ~ Classification_name)
anova(lm_LAI_Spring)

lm_SRTM_elevation <- lm(data=Data_1, SRTM_elevation ~ MAM + AOP + GSOH +MAM*AOP+ MAM*GSOH+ AOP*GSOH +Classification_name)
lm_SRTM_elevation <- lm(data=Data_1, SRTM_elevation ~ Classification_name)
anova(lm_SRTM_elevation)

lm_CHELSA_Interann_Temp <- lm(data=Data_1, CHELSA_Interann_Temp ~ MAM + AOP + GSOH +MAM*AOP+ MAM*GSOH+ AOP*GSOH +Classification_name)
lm_CHELSA_Interann_Temp <- lm(data=Data_1, CHELSA_Interann_Temp ~ Classification_name)
anova(lm_CHELSA_Interann_Temp)

lm_CHELSA_BIO4 <- lm(data=Data_1, CHELSA_BIO4 ~ MAM + AOP + GSOH +MAM*AOP+ MAM*GSOH+ AOP*GSOH +Classification_name)
lm_CHELSA_BIO4 <- lm(data=Data_1, CHELSA_BIO4 ~ Classification_name)
anova(lm_CHELSA_BIO4)

lm_CHELSA_BIO6 <- lm(data=Data_1, CHELSA_BIO6 ~ MAM + AOP + GSOH +MAM*AOP+ MAM*GSOH+ AOP*GSOH +Classification_name)
lm_CHELSA_BIO6 <- lm(data=Data_1, CHELSA_BIO6 ~ Classification_name)
anova(lm_CHELSA_BIO6)

lm_CHELSA_BIO7 <- lm(data=Data_1, CHELSA_BIO7 ~ MAM + AOP + GSOH +MAM*AOP+ MAM*GSOH+ AOP*GSOH +Classification_name)
lm_CHELSA_BIO7 <- lm(data=Data_1, CHELSA_BIO7 ~ Classification_name)
anova(lm_CHELSA_BIO7)

lm_CHELSA_BIO11 <- lm(data=Data_1, CHELSA_BIO11 ~ MAM + AOP + GSOH +MAM*AOP+ MAM*GSOH+ AOP*GSOH +Classification_name)
lm_CHELSA_BIO11 <- lm(data=Data_1, CHELSA_BIO11 ~ Classification_name)
anova(lm_CHELSA_BIO11)

lm_WC2_BIO7 <- lm(data=Data_1, WC2_BIO7 ~ MAM + AOP + GSOH +MAM*AOP+ MAM*GSOH+ AOP*GSOH +Classification_name)
lm_WC2_BIO7 <- lm(data=Data_1, WC2_BIO7 ~ Classification_name)
anova(lm_WC2_BIO7)

lm_WC2_BIO6 <- lm(data=Data_1, WC2_BIO6 ~ MAM + AOP + GSOH +MAM*AOP+ MAM*GSOH+ AOP*GSOH +Classification_name)
lm_WC2_BIO6 <- lm(data=Data_1, WC2_BIO6 ~ Classification_name)
anova(lm_WC2_BIO6)

lm_WC2_BIO12 <- lm(data=Data_1, WC2_BIO12 ~ MAM + AOP + GSOH +MAM*AOP+ MAM*GSOH+ AOP*GSOH +Classification_name)
lm_WC2_BIO12 <- lm(data=Data_1, WC2_BIO12 ~ Classification_name)
anova(lm_WC2_BIO12)

lm_WC2_BIO13 <- lm(data=Data_1, WC2_BIO13 ~ MAM + AOP + GSOH +MAM*AOP+ MAM*GSOH+ AOP*GSOH + Classification_name)
lm_WC2_BIO13 <- lm(data=Data_1, WC2_BIO13 ~ Classification_name)
anova(lm_WC2_BIO13)

lm_WC2_BIO16 <- lm(data=Data_1, WC2_BIO16 ~ MAM + AOP + GSOH +MAM*AOP+ MAM*GSOH+ AOP*GSOH +Classification_name)
lm_WC2_BIO16 <- lm(data=Data_1, WC2_BIO16 ~ Classification_name)
anova(lm_WC2_BIO16)

lm_WC2_BIO19 <- lm(data=Data_1, WC2_BIO19 ~ MAM + AOP + GSOH +MAM*AOP+ MAM*GSOH+ AOP*GSOH +Classification_name)
lm_WC2_BIO19 <- lm(data=Data_1, WC2_BIO19 ~ Classification_name)
anova(lm_WC2_BIO19)

ggplot(Data_1, aes(LAI_Spring, BZO_ratio, color=(CS=="CS76778"), ))+
  geom_point(alpha = 5/10)+theme_bw()+  scale_color_manual(values=c('grey','black'))

#Stat just for Iberian penincula:
#Grouping by geography:
Data_1$GEO <- NA
Data_1$GEO[which(Data_1$Country == "ESP")] <- "Iberian_Penincula"
Data_1$GEO[which(Data_1$Country == "POR")] <- "Iberian_Penincula"
Data_1$GEO[which(Data_1$Country == "USA")] <- "USA"
Data_1$GEO[which(Data_1$Country == "CAN")] <- "USA"
Data_1$GEO[is.na(Data_1$GEO)] <- "Europe"

#Separating the GEO:

Data_1$GEO_1 <- paste("G", Data_1$GEO, sep="")
Data_1$GEO_1 <- as.factor(Data_1$GEO_1)
Data_1_GEO <- split(Data_1, Data_1$GEO_1)
list2env(Data_1_GEO, envir=.GlobalEnv)
rm(Data_1_GEO)

Data_1GIberian_Penincula <- droplevels.data.frame(GIberian_Penincula)
Data_1GUSA <- droplevels.data.frame(GUSA)
Data_1GEurope <- droplevels.data.frame(GEurope)

print(mymap)+
  geom_point(data=Data_1, aes(x=Long, y=Lat, color=GEO_1), alpha = 7/10, size=2) +
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
  scale_color_manual(values = c("black", "blue"))

ggplot(Data_1GIberian_Penincula, aes(x=factor(1), fill=Classification_name))+
  geom_bar(width = 1)+
  coord_polar("y")+ theme_void()+
  labs(x = NULL, y = NULL, fill = NULL, title = "Chemotypes - Iberian Peninsula")+
    theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black"))+
  theme(plot.margin = unit(c(1, 9, 8, 5), "cm"))


colnames(Data_1)
lm_LAI_Spring_I <- lm(data=Data_1GIberian_Penincula, LAI_Spring ~ MAM + AOP + GSOH +MAM*AOP+ MAM*GSOH+ AOP*GSOH )
lm_LAI_Spring_I <- lm(data=Data_1GIberian_Penincula, LAI_Spring ~Classification_name )
lm_LAI_Spring <- lm(data=Data_1, LAI_Spring ~ Classification_name)
anova(lm_LAI_Spring_I)

lm_SRTM_elevation_I <- lm(data=Data_1GIberian_Penincula, SRTM_elevation ~ MAM + AOP + GSOH +MAM*AOP+ MAM*GSOH+ AOP*GSOH )
lm_SRTM_elevation_I <- lm(data=Data_1GIberian_Penincula, SRTM_elevation ~ Classification_name)
lm_SRTM_elevation <- lm(data=Data_1, SRTM_elevation ~ Classification_name)
anova(lm_SRTM_elevation_I)

lm_CHELSA_Interann_Temp_I <- lm(data=Data_1GIberian_Penincula, CHELSA_Interann_Temp ~ MAM + AOP + GSOH +MAM*AOP+ MAM*GSOH+ AOP*GSOH)
lm_CHELSA_Interann_Temp_I <- lm(data=Data_1GIberian_Penincula, CHELSA_Interann_Temp ~ Classification_name)
lm_CHELSA_Interann_Temp <- lm(data=Data_1, CHELSA_Interann_Temp ~ Classification_name)
anova(lm_CHELSA_Interann_Temp_I)

lm_CHELSA_BIO4_I <- lm(data=Data_1GIberian_Penincula, CHELSA_BIO4 ~ MAM + AOP + GSOH +MAM*AOP+ MAM*GSOH+ AOP*GSOH)
lm_CHELSA_BIO4_I <- lm(data=Data_1GIberian_Penincula, CHELSA_BIO4 ~ Classification_name)
lm_CHELSA_BIO4 <- lm(data=Data_1, CHELSA_BIO4 ~ Classification_name)
anova(lm_CHELSA_BIO4_I)

lm_CHELSA_BIO6_I <- lm(data=Data_1GIberian_Penincula, CHELSA_BIO6 ~ MAM + AOP + GSOH +MAM*AOP+ MAM*GSOH+ AOP*GSOH)
lm_CHELSA_BIO6_I <- lm(data=Data_1GIberian_Penincula, CHELSA_BIO6 ~ Classification_name)
lm_CHELSA_BIO6 <- lm(data=Data_1, CHELSA_BIO6 ~ Classification_name)
anova(lm_CHELSA_BIO6_I)

lm_CHELSA_BIO7_I <- lm(data=Data_1GIberian_Penincula, CHELSA_BIO7 ~ MAM + AOP + GSOH +MAM*AOP+ MAM*GSOH+ AOP*GSOH)
lm_CHELSA_BIO7_I <- lm(data=Data_1GIberian_Penincula, CHELSA_BIO7 ~ Classification_name)
lm_CHELSA_BIO7 <- lm(data=Data_1, CHELSA_BIO7 ~ Classification_name)
anova(lm_CHELSA_BIO7_I)

lm_CHELSA_BIO11_I <- lm(data=Data_1GIberian_Penincula, CHELSA_BIO11 ~ MAM + AOP + GSOH +MAM*AOP+ MAM*GSOH+ AOP*GSOH)
lm_CHELSA_BIO11_I <- lm(data=Data_1GIberian_Penincula, CHELSA_BIO11 ~ Classification_name)
lm_CHELSA_BIO11 <- lm(data=Data_1GIberian_Penincula, CHELSA_BIO11 ~ Classification_name+ MAM+AOP+GSOH)
anova(lm_CHELSA_BIO11_I)

Data_1GIberian_Penincula[is.na(Data_1GIberian_Penincula)] <- ""
Tlm_CHELSA_BIO11 <- glht(lm_CHELSA_BIO11_I, linfct = mcp(Classification_name = "Tukey"))
cld(Tlm_CHELSA_BIO11)


lm_WC2_BIO7_I <- lm(data=Data_1GIberian_Penincula, WC2_BIO7 ~ MAM + AOP + GSOH +MAM*AOP+ MAM*GSOH+ AOP*GSOH)
lm_WC2_BIO7_I <- lm(data=Data_1GIberian_Penincula, WC2_BIO7 ~ Classification_name)
lm_WC2_BIO7 <- lm(data=Data_1, WC2_BIO7 ~ Classification_name)
anova(lm_WC2_BIO7_I)

lm_WC2_BIO6_I <- lm(data=Data_1GIberian_Penincula, WC2_BIO6 ~ MAM + AOP + GSOH +MAM*AOP+ MAM*GSOH+ AOP*GSOH)
lm_WC2_BIO6_I <- lm(data=Data_1GIberian_Penincula, WC2_BIO6 ~ Classification_name)
lm_WC2_BIO6 <- lm(data=Data_1, WC2_BIO6 ~ Classification_name)
anova(lm_WC2_BIO6_I)

lm_WC2_BIO12_I <- lm(data=Data_1GIberian_Penincula, WC2_BIO12 ~ MAM + AOP + GSOH +MAM*AOP+ MAM*GSOH+ AOP*GSOH)
lm_WC2_BIO12_I <- lm(data=Data_1GIberian_Penincula, WC2_BIO12 ~ Classification_name)
lm_WC2_BIO12 <- lm(data=Data_1, WC2_BIO12 ~ Classification_name)
anova(lm_WC2_BIO12_I)

lm_WC2_BIO13_I <- lm(data=Data_1GIberian_Penincula, WC2_BIO13 ~ MAM + AOP + GSOH +MAM*AOP+ MAM*GSOH+ AOP*GSOH)
lm_WC2_BIO13_I <- lm(data=Data_1GIberian_Penincula, WC2_BIO13 ~ Classification_name)
lm_WC2_BIO13 <- lm(data=Data_1, WC2_BIO13 ~ Classification_name)
anova(lm_WC2_BIO13_I)

lm_WC2_BIO16_I <- lm(data=Data_1GIberian_Penincula, WC2_BIO16 ~ MAM + AOP + GSOH +MAM*AOP+ MAM*GSOH+ AOP*GSOH )
lm_WC2_BIO16_I <- lm(data=Data_1GIberian_Penincula, WC2_BIO16 ~ Classification_name )
lm_WC2_BIO16_I <- lm(data=Data_1GIberian_Penincula, WC2_BIO16 ~ Classification_name)
anova(lm_WC2_BIO16_I)

lm_WC2_BIO19_I <- lm(data=Data_1GIberian_Penincula, WC2_BIO19 ~ MAM + AOP + GSOH +MAM*AOP+ MAM*GSOH+ AOP*GSOH)
lm_WC2_BIO19_I <- lm(data=Data_1GIberian_Penincula, WC2_BIO19 ~ Classification_name)
lm_WC2_BIO19_I <- lm(data=Data_1GIberian_Penincula, WC2_BIO19 ~ Classification_name)
anova(lm_WC2_BIO19_I)

lm_SoilGrids_BDRICM_I <- lm(data=Data_1GIberian_Penincula, GAEZ_Nutrient_availability ~ MAM + AOP + GSOH +MAM*AOP+ MAM*GSOH+ AOP*GSOH +Classification_name)
lm_WC2_BIO19_I <- lm(data=Data_1GIberian_Penincula, WC2_BIO19 ~ Classification_name)
anova(lm_SoilGrids_BDRICM_I)


Tlm_WC2_BIO19_I <- glht(lm_WC2_BIO7_I, linfct = mcp(Classification_name = "Tukey"))
cld(Tlm_WC2_BIO19_I)

lm_CHELSA_BIO11_I <- lm(data=Data_1GIberian_Penincula, CHELSA_BIO11 ~ MAM + AOP + GSOH +MAM*AOP+ MAM*GSOH+ AOP*GSOH +Classification_name)
anova(lm_CHELSA_BIO11_I)

#Logistic regression:
install.packages("aod")
library(aod)

Data_1GIberian_Penincula$Classification_name <- factor(Data_1GIberian_Penincula$Classification_name)
mylogit <- glm(WC2_BIO7 ~ MAM + AOP + GSOH  +Classification_name, data = Data_1GIberian_Penincula, family = "binomial")




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

a1 <- manova(cbind(WC2_BIO13, WC2_BIO6) ~ Classification_name, data = Data_1GIberian_Penincula)
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
a22a <- manova(cbind(CHELSA_BIO11, WC2_BIO16, SRTM_elevation) ~ MAM+AOP+GSOH+ MAM*AOP+ MAM*GSOH+ AOP*GSOH, data = Data_1GIberian_Penincula)
summary(a22a)
summary.aov(a22a)


a23 <- manova(cbind(CHELSA_BIO13, WC2_BIO6, SRTM_elevation) ~ Classification_name, data = Data_1GIberian_Penincula)
summary(a23)
summary.aov(a23)
a23a <- manova(cbind(CHELSA_BIO13, WC2_BIO6, SRTM_elevation) ~  MAM+AOP+GSOH+ MAM*AOP+ MAM*GSOH+ AOP*GSOH, data = Data_1GIberian_Penincula)
summary(a23a)
summary.aov(a23a)
SRTM_elevation
CHELSA_BIO6

ggplot(data=Data_1GIberian_Penincula, aes(Classification_name, SRTM_elevation, fill=Classification_name))+ 
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


#The rest of Europe:

a24 <- manova(cbind(CHELSA_BIO11, WC2_BIO16, SRTM_elevation) ~ Classification_name, data = Data_1GEurope)
summary(a24)
summary.aov(a24)
a24b <- manova(cbind(CHELSA_BIO11, WC2_BIO16) ~ Classification_name, data = Data_1GEurope)
cld(emmeans(a24b, ~Classification_name))
a24a <- manova(cbind(CHELSA_BIO11, WC2_BIO16, SRTM_elevation) ~ MAM+AOP+GSOH+ MAM*AOP+ MAM*GSOH+ AOP*GSOH, data = Data_1GEurope)
summary(a24a)
summary.aov(a24a)


a25 <- manova(cbind(WC2_BIO13, WC2_BIO6, SRTM_elevation) ~ Classification_name, data = Data_1GEurope)
summary(a25)
summary.aov(a25)
a25b <- manova(cbind(WC2_BIO13, WC2_BIO6) ~ Classification_name, data = Data_1GEurope)
cld(emmeans(a25b, ~Classification_name))
a25a <- manova(cbind(WC2_BIO13, WC2_BIO6, SRTM_elevation) ~  MAM+AOP+GSOH+ MAM*AOP+ MAM*GSOH+ AOP*GSOH, data = Data_1GEurope)
summary(a25a)
summary.aov(a25a)

ggplot(data=Data_1GEurope, aes(Classification_name, WC2_BIO6, fill=Classification_name))+ 
  theme_minimal() + 
  geom_bar(aes(fill=Classification_name), stat = "summary", fun.y = "mean", position = position_dodge(), width=0.7)+ 
  geom_jitter(aes(colour = Classification_name), position = position_jitter(width = 0.1), alpha = 0.2) +  
  ggtitle("")+
  scale_fill_manual(values = c("orange", "yellow", "green2", "magenta", "skyblue", "blue", "black", "red3"))+
  ylab("WorldClim BIO6 = Min temperature of coldest month") +
  xlab("Chemotype") +
  theme(plot.margin = unit(c(1, 8, 1, 2), "cm"))

#USA:

a24 <- manova(cbind(CHELSA_BIO11, WC2_BIO16, SRTM_elevation) ~ Classification_name, data = Data_1GUSA)
summary(a24)
summary.aov(a24)
a24b <- manova(cbind(CHELSA_BIO11, WC2_BIO16) ~ Classification_name, data = Data_1GUSA)
cld(emmeans(a24b, ~Classification_name))
a24a <- manova(cbind(CHELSA_BIO11, WC2_BIO16, SRTM_elevation) ~ MAM+AOP+GSOH+ MAM*AOP+ MAM*GSOH+ AOP*GSOH, data = Data_1GUSA)
summary(a24a)
summary.aov(a24a)


a25 <- manova(cbind(WC2_BIO13, WC2_BIO6, SRTM_elevation) ~ Classification_name, data = Data_1GUSA)
summary(a25)
summary.aov(a25)
a25b <- manova(cbind(WC2_BIO13, WC2_BIO6) ~ Classification_name, data = Data_1GUSA)
cld(emmeans(a25b, ~Classification_name))
a25a <- manova(cbind(WC2_BIO13, WC2_BIO6, SRTM_elevation) ~  MAM+AOP+GSOH+ MAM*AOP+ MAM*GSOH+ AOP*GSOH, data = Data_1GUSA)
summary(a25a)
summary.aov(a25a)



ggplot(data=Data_1GUSA, aes(Classification_name, WC2_BIO6, fill=Classification_name))+ 
  theme_minimal() + 
  geom_bar(aes(fill=Classification_name), stat = "summary", fun.y = "mean", position = position_dodge(), width=0.7)+ 
  geom_jitter(aes(colour = Classification_name), position = position_jitter(width = 0.1), alpha = 0.2) +  
  ggtitle("")+
  scale_fill_manual(values = c("orange", "yellow", "green2", "magenta", "blue", "red3"))+
  ylab("WorldClim BIO6 = Min temperature of coldest month") +
  xlab("Chemotype") +
  theme(plot.margin = unit(c(1, 8, 1, 2), "cm"))

#Correlation plots:
ggplot(Data_1GIberian_Penincula, aes(x=WC2_BIO7, y=WC2_BIO12, color=Classification_name)) + geom_point(size=2)+
  scale_color_manual(values = c("green2", "magenta", "blue", "black", "red3"))+
  theme_bw()+
  ylab("WorldClim BIO12 = Annual precipitation") +
  xlab("WorldClim BIO7 = Temperature annual range")

ggplot(Data_1GIberian_Penincula, aes(x=CHELSA_BIO11, y=WC2_BIO16, color=Classification_name)) + geom_point(size=2)+
scale_color_manual(values = c("green2", "magenta", "blue", "black", "red3"))+
  theme_bw()+
  ylab("WorldClim BIO16 = Precipitation of wettest quarter") +
  xlab("CHELSA BIO11 = Mean temperature of coldest quarter")

ggplot(Data_1GIberian_Penincula, aes(x=WC2_BIO1, y=WC2_BIO13, color=Classification_name)) + 
  geom_point(size=2)+
  scale_color_manual(values = c("green2", "magenta", "blue", "black", "red3"))+
  theme_bw()+
  ylab("WorldClim BIO13 = Precipitation of wettest month") +
  xlab("WorldClim BIO6 = Min temperature of coldest month")


ggplot(Data_1GEurope, aes(x=CHELSA_BIO11, y=WC2_BIO16, color=Classification_name)) + geom_point(size=2)+
  scale_color_manual(values = c("orange", "yellow", "green2", "magenta", "skyblue", "blue", "black", "red3"))+
  theme_bw()+
  ylab("WorldClim BIO16 = Precipitation of wettest quarter") +
  xlab("CHELSA BIO11 = Mean temperature of coldest quarter")

ggplot(Data_1GEurope, aes(x=WC2_BIO1, y=WC2_BIO13, color=Classification_name)) + 
  geom_point(size=2)+
  scale_color_manual(values = c("orange", "yellow", "green2", "magenta", "skyblue", "blue", "black", "red3"))+
  theme_bw()+
  ylab("WorldClim BIO13 = Precipitation of wettest month") +
  xlab("WorldClim BIO6 = Min temperature of coldest month")

ggplot(Data_1GUSA, aes(x=CHELSA_BIO11, y=WC2_BIO16, color=Classification_name)) + geom_point(size=2)+
  scale_color_manual(values = c("orange", "yellow", "green2", "magenta", "blue", "red3"))+
  theme_bw()+
  ylab("WorldClim BIO16 = Precipitation of wettest quarter") +
  xlab("CHELSA BIO11 = Mean temperature of coldest quarter")

ggplot(Data_1GUSA, aes(x=WC2_BIO1, y=WC2_BIO13, color=Classification_name)) + 
  geom_point(size=2)+
  scale_color_manual(values = c("orange", "yellow", "green2", "magenta", "blue", "red3"))+
  theme_bw()+
  ylab("WorldClim BIO13 = Precipitation of wettest month") +
  xlab("WorldClim BIO6 = Min temperature of coldest month")
