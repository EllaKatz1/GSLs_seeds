setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA")
Accessions_data <- read.table(file="Master_GSL.csv", header=T, sep=",")


#PCA on all GSLs:
colnames(Accessions_data)
details_1<-Accessions_data[,c(2,50:53)]

data_PCA_1 <- Accessions_data[,c(5:27)]

res.pca <- prcomp(data_PCA_1, center=T, scale=T)
PCscores <- as.data.frame(res.pca$x)

data_PC_1 <- cbind(Accessions_data, PCscores)
colnames(data_PC_1)

ggplot(data_PC_1, aes(PC1, PC2))+
  geom_point()+theme_bw()+ ggtitle("Principal Component Analysis - GSLs") 


ggplot(data_PC_1, aes(PC1, PC2, color=Ref, shape=Ref))+
  geom_point()+theme_bw()+ ggtitle("Principal Component Analysis") +
  scale_color_manual(values = c("grey", "black", "black", "black"))


#ggsave("PCA.png", dpi=300)

#Creating the PCA barplot:

pca.var <- res.pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)

barplot(pca.var.per, main="Scree Plot - GSLs", xlab="Principal Component", ylab="Percent Variation")

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
fviz_contrib(res.pca, choice="var", axes=1, top=20, title="Contribution of variables - GSLs")

ggplot(data_PC_1, aes(PC1, PC2, color=AOP))+
  geom_point()+theme_bw()+ ggtitle("Principal Component Analysis")+
  scale_color_manual(values = c("magenta1", "black", "green3", "gold" ))

ggplot(data_PC_1, aes(PC1, PC2, color=Elong))+
  geom_point()+theme_bw()+ ggtitle("Principal Component Analysis")+
  scale_color_manual(values = c("orange","blue"))

ggplot(data_PC_1, aes(PC1, PC2, color=GSOH_1))+
  geom_point()+theme_bw()+ ggtitle("Principal Component Analysis")+
  scale_color_manual(values = c("black","violetred2"))


ggplot(data_PC_1, aes(PC1, PC2, color=Classification_name))+
  geom_point()+theme_bw()+ ggtitle("Principal Component Analysis")+
  scale_color_manual(values = c("orange", "yellow", "green2", "magenta", "skyblue",  "blue",  "black", "red3"))

Data_2 <- merge(data_PC_1, details_1, by="CS")
Data_2 <- droplevels.data.frame(Data_2)
Data_2 <- unique(Data_2)
colnames(Data_2)


#PCA on short chained GSLs:
colnames(Accessions_data)


data_PCA_2 <- Accessions_data[,c(5:10,12,17,20)]

res.pca <- prcomp(data_PCA_2, center=T, scale=T)
PCscores <- as.data.frame(res.pca$x)

data_PC_2 <- cbind(Accessions_data, PCscores)
colnames(data_PC_2)

ggplot(data_PC_2, aes(PC1, PC2))+
  geom_point()+theme_bw()+ ggtitle("PCA - SC GSLs")+
  geom_point(color='darkblue')


ggplot(data_PC_2, aes(PC1, PC2, color=Ref, shape=Ref))+
  geom_point()+theme_bw()+ ggtitle("PCA - SC GSLs") +
  scale_color_manual(values = c("grey", "black", "black", "black"))


#ggsave("PCA.png", dpi=300)

#Creating the PCA barplot:

pca.var <- res.pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)

barplot(pca.var.per, main="Scree Plot - SC GSLs", xlab="Principal Component", ylab="Percent Variation")

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
fviz_contrib(res.pca, choice="var", axes=1, top=20, title="Contribution of variables - SC GSLs")

ggplot(data_PC_2, aes(PC1, PC2, color=AOP))+
  geom_point()+theme_bw()+ ggtitle("PCA - AOP status")+
  scale_color_manual(values = c("purple","black", "green", "orange"))

ggplot(data_PC_2, aes(PC1, PC2, color=Elong))+
  geom_point()+theme_bw()+ ggtitle("PCA - Elong status")+
  scale_color_manual(values = c("blue","red"))

ggplot(data_PC_2, aes(PC1, PC2, color=GSOH_1))+
  geom_point()+theme_bw()+ ggtitle("PCA - GSOH status")+
  scale_color_manual(values = c("violetred2","black"))


ggplot(data_PC_2, aes(PC1, PC2, color=Classification_name))+
  geom_point()+theme_bw()+ ggtitle("PCA - Chemotypes")+
  scale_color_manual(values = c("orange", "yellow", "green2", "magenta", "skyblue",  "blue",  "black", "red3"))

setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Enviroment")
Enviromental_data <- read.table(file="shiny climatesd short.csv", header=T, sep=",")
colnames(Enviromental_data)
Env<-Enviromental_data[,c(1,2)]
Data_Env <- merge(data_PC_2, Env, by="CS")

ggplot(Data_Env , aes(PC1, PC2, color=group))+
  geom_point()+theme_bw()+ ggtitle("PCA - group")


Data_2 <- merge(data_PC_2, details_1, by="CS")
Data_2 <- droplevels.data.frame(Data_2)
Data_2 <- unique(Data_2)
colnames(Data_2)

#setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny")
#write.csv(Data_2, file = "Data_PCA.csv")

#Linear models:
colnames(data_PC_2)
lm_AOP <- lm(data=data_PC_2, Alk_OH_ratio ~ Long + Lat + Long*Lat)
anova(lm_AOP)

lm_Elong <- lm(data=data_PC_2, C3ratio_emmeans ~ Long + Lat + Long*Lat)
anova(lm_Elong)

lm_GSOH <- lm(data=data_PC_2, GSOH ~ Long + Lat + Long*Lat)
anova(lm_GSOH)

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
  geom_point(data=Data_2, aes(x=Long.y, y=Lat.y, color=PC2), alpha = 7/10, size=2) +
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
     scale_color_gradientn(colours = rainbow(5))
  
   scale_colour_gradientn(colours = terrain.colors(10))  
scale_colour_gradient(low = "blue", high = "red")

print(mymap)+
  geom_point(data=Data_2, aes(x=Long.y, y=Lat.y, color=Classification_name), alpha = 7/10, size=2) +
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
    scale_color_manual(values = c("orange", "yellow", "green2", "magenta", "skyblue",  "blue",  "black", "red3"))

print(mymap)+
  geom_point(data=Data_2, aes(x=Long.y, y=Lat.y, color=GSOH_1), alpha = 7/10, size=2) +
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
  scale_color_manual(values = c("black","violetred2"))



library(plyr)

count(Data_2$Classification_name)
