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
#install.packages("RJSONIO")
library(RJSONIO)
#install.packages("googleway") 
library("googleway")

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


Data_01 <- mutate(data, C3= (X3OHP + X3MSO + Allyl + X3MT),
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

#PCA on GSLs:
colnames(Data_1)
data_PCA_2 <- Data_1[,c(4:26)]

res.pca <- prcomp(data_PCA_2, center=T, scale=T)
PCscores <- as.data.frame(res.pca$x)

data_PC_2 <- cbind(Data_1, PCscores)


ggplot(data_PC_2, aes(PC1, PC2, color=C3ratio_emmeans, shape=Ref))+
  geom_point()+theme_bw()+ ggtitle("Principal Component Analysis, by C3 ratio") 


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





#Col:
ggplot(Data_1, aes(OH_emmeans, Alk_emmeans, color=(CS=="CS76778"), ))+
  geom_point(alpha = 5/10)+theme_bw() +  scale_color_manual(values=c('grey','red'))

#Ler:
ggplot(Data_1, aes(OH_emmeans, Alk_emmeans, color=(CS=="CS77021"), ))+
  geom_point(alpha = 5/10)+theme_bw() +  scale_color_manual(values=c('grey','red'))

#Cvi:
ggplot(Data_1, aes(OH_emmeans, Alk_emmeans, color=(CS=="CS76789"), ))+
  geom_point(alpha = 5/10)+theme_bw() +  scale_color_manual(values=c('grey','red'))

#color by Alk OH ratio
ggplot(Data_1, aes(OH_emmeans, Alk_emmeans, color=Alk_OH_ratio_emmeans))+
  geom_point(alpha = 5/10)+theme_bw() +scale_color_gradientn(colours = rainbow(9))

ggplot(Data_1, aes(OH_emmeans, Alk_emmeans, color=C3ratio_emmeans))+
  geom_point()+theme_bw()


qplot(Data_1$C3ratio_emmeans, geom="histogram") +theme_bw()

#C7 ratio:
colnames(Data_1)

Data_C7 <- mutate(Data_1, C7ratio= (X7MSO+X7MT)/(X7MSO+X7MT+X8MSO+X8MT+X8MTder))
ggplot(Data_C7, aes(C7ratio, C3ratio_emmeans, color=C3ratio_emmeans))+
  geom_point()+theme_bw()

#Data_2 <- mutate(Data_1, Alk_OH_ratio_emmeans=(Alk_emmeans+0.00001)/(Alk_emmeans+OH_emmeans), 
#Null_1=(MSO_emmeans)/(MSO_emmeans+OH_emmeans+Alk_emmeans))

#Data_2$Alk_OH<-cut(Data_2$Alk_OH_ratio_emmeans, c(0, 0.00002, 0.1, 0.6, 1))
#Data_2$Alk_OH <- Data_2$Alk_OH
#levels(Data_2$Alk_OH) <- c("OH", "MSO", "?", "Alk")

#ggplot(Data_2, aes(OH_emmeans, Alk_emmeans, color=Alk_OH))+geom_point(alpha = 5/10)+theme_bw() 

#Temporary list:
colnames(Data_1)
Data_temp <- Data_1[,c(2, 33:36, 40)] 

Data_traits_1 <- merge(details_1, Data_temp, by="CS")
Data_traits_1 <- droplevels.data.frame(Data_traits_1)
Data_traits <- unique(Data_traits_1)

write.csv(Data_traits, file = "Data_traits.csv")

#Chemotyping:

colnames(Data_1)
ggplot(Data_1, aes(OH_emmeans, Alk_emmeans, color=C3ratio_emmeans))+
  geom_point(alpha = 5/10)+theme_bw()+scale_color_gradientn(colours = rainbow(7))

#3D plot:
library(scatterplot3d)
scatterplot3d(Data_1$OH_emmeans,Data_1$MSO_emmeans,Data_1$Alk_emmeans, main="3D Scatterplot")

Surface_data =interp(Data_1$OH_emmeans,Data_1$MSO_emmeans,Data_1$Alk_emmeans)

#install.packages('plot3D')
#library(plot3D)
#install.packages('interp')
library(interp)

#install.packages("plotly")
library(plotly)
library(rgl)

plot3d(Data_1$OH_emmeans,Data_1$MSO_emmeans,Data_1$Alk_emmeans, type="n", 
       xlab="OH", zlab="MSO", ylab="Alk")

surface3d(Data_1$OH_emmeans,Data_1$MSO_emmeans,Data_1$Alk_emmeans, color=c("grey"))
points3d(Surface_data)

Surface_data =interp(Data_1$OH_emmeans, Data_1$MSO_emmeans, Data_1$Alk_emmeans)

LSA3D <- plot_ly(x = Surface_data$x, y = Surface_data$y, z = 
                   Surface_data$z) %>%
  add_surface() %>%
  layout(
    title="",
    scene=list(
      xaxis = list(title = "OH"),
      yaxis = list(title = "MSO"),
      zaxis = list(title = "Alk")
    ))
LSA3D


plot_ly(Data_1, x = ~OH_emmeans, y = ~MSO_emmeans, z = ~Alk_emmeans, color = ~ BZO, symbol = ~Ref, symbols = c('o','x','x','x'), size=10, alpha = 0.5)%>%
  add_markers() %>% 
  layout(scene = list(xaxis = list(title = 'OH'),
                      yaxis = list(title = 'MSO'),
                      zaxis = list(title = 'Alk'))) %>% 
  add_text(Data_1, x = ~OH_emmeans, y = ~MSO_emmeans, z = ~Alk_emmeans) 

#With Ref: 
plot_ly(Data_1, x = ~OH_emmeans, y = ~MSO_emmeans, z = ~Alk_emmeans, color = ~ C3ratio_emmeans, 
        symbol = ~Ref, symbols = c('circle','x','x','x'), size=10, alpha = 0.7)%>%
  add_markers() %>% 
    layout(scene = list(xaxis = list(title = 'OH'),
                      yaxis = list(title = 'MSO'),
                      zaxis = list(title = 'Alk'))) %>% 
    add_text(Data_1, x = ~OH_emmeans, y = ~MSO_emmeans, z = ~Alk_emmeans) 

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

plot_ly(Data_1, x = ~OH_emmeans, y = ~MSO_emmeans, z = ~Alk_emmeans, color = ~ AOP, 
        symbol = ~Ref, symbols = c('circle','x','x','x'), size=10, alpha = 0.8)%>%
  add_markers() %>% 
  layout(scene = list(xaxis = list(title = 'OH'),
                      yaxis = list(title = 'MSO'),
                      zaxis = list(title = 'Alk'))) %>% 
  add_text(Data_1, x = ~OH_emmeans, y = ~MSO_emmeans, z = ~Alk_emmeans) 


#MAM classification:

qplot(Data_1$C3ratio_emmeans, geom="histogram") +theme_bw()
ggplot(data=Data_1, aes(Data_1$C3ratio_emmeans)) + geom_histogram()+theme_bw()+
  ylab("# of accessions") +
  xlab("C3/C4 ratio")

#C7 ratio:
Data_C7 <- mutate(Data_1, C7ratio= (X7MSO+X7MT)/(X7MSO+X7MT+X8MSO+X8MT+X8MTder))
ggplot(Data_C7, aes(C7ratio, C3ratio_emmeans, color=C3ratio_emmeans))+
  geom_point()+theme_bw()

colnames(Data_1)
Data_1$Elong<-cut(Data_1$C3ratio_emmeans, c(-0.1, 0.5, 1.1))
Data_1$Elong <- Data_1$Elong
levels(Data_1$Elong) <- c("C4", "C3")


plot_ly(Data_1, x = ~OH_emmeans, y = ~MSO_emmeans, z = ~Alk_emmeans, color = ~ Elong, 
        symbol = ~AOP, symbols = c('circle','x','square','cross'), size=10, alpha = 0.8)%>%
  add_markers() %>% 
  layout(scene = list(xaxis = list(title = 'OH'),
                      yaxis = list(title = 'MSO'),
                      zaxis = list(title = 'Alk'))) %>% 
  add_text(Data_1, x = ~OH_emmeans, y = ~MSO_emmeans, z = ~Alk_emmeans) 

#GSOH
colnames(Data_1)
qplot(Data_1$GSOH_emmeans, geom="histogram") +theme_bw()
ggplot(data=Data_1, aes(Data_1$GSOH_emmeans)) + geom_histogram()+theme_bw()+
  ylab("# of accessions") +
  xlab("OH_Butenyl  ratio")

Data_1$GSOH_1<-cut(Data_1$GSOH, c(-0.1, 0.1, 2))
Data_1$ GSOH_1<- as.factor(Data_1$ GSOH_1)
levels(Data_1$GSOH_1) <- c("NF", "F")

#Saving data:
colnames(Data_1)
Classification <- Data_1[,c(2,34:44)] 
write.csv(Classification, file = "Data_traits.csv")


plot_ly(Data_1, x = ~OH_emmeans, y = ~MSO_emmeans, z = ~Alk_emmeans, color = ~ GSOH_1, 
        symbol = ~AOP, symbols = c('circle','x','square','cross'), size=10, alpha = 0.8)%>%
  add_markers() %>% 
  layout(scene = list(xaxis = list(title = 'OH'),
                      yaxis = list(title = 'MSO'),
                      zaxis = list(title = 'Alk'))) %>% 
  add_text(Data_1, x = ~OH_emmeans, y = ~MSO_emmeans, z = ~Alk_emmeans) 


#Things to work on: the differentiation between Allyl to Butenyl. 


Data_1$Key <- as.factor(paste(Data_1$Elong, Data_1$AOP, Data_1$GSOH_1,  sep="_"))
Data_1$Classification <- Data_1$Key
nlevels(Data_1$Key)

levels(Data_1$Classification) <- c(1,2,3,4,5,6,7,8,9,10,11,12,13)
Data_1$Classification_name <- Data_1$Classification

#Divisions by (this order): MAM  (3/4), AOP(Alk/MSO/OH), GSOH(+/-)
levels(Data_1$Classification_name) <- c("Allyl", "Allyl", "OH-But", "?", "3MSO", 
                                        "3MSO", "3OHP", "3OHP", "OH-But", "Butenyl", "4MSO", "4MSO", "4OHB")


plot_ly(Data_1, x = ~OH_emmeans, y = ~MSO_emmeans, z = ~Alk_emmeans, color = ~ Classification_name, 
        symbol = ~Ref, symbols = c('circle','x','x','x'), size=10, alpha = 0.8)%>%
  add_markers() %>% 
  layout(scene = list(xaxis = list(title = 'OH'),
                      yaxis = list(title = 'MSO'),
                      zaxis = list(title = 'Alk'))) %>% 
  add_text(Data_1, x = ~OH_emmeans, y = ~MSO_emmeans, z = ~Alk_emmeans) 


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
  
#PCA with GSLs:
colnames(Data_1)
data_PCA_4 <- Data_1[,c(4:26)]

res.pca <- prcomp(data_PCA_4, center=T, scale=T)
PCscores <- as.data.frame(res.pca$x)

data_PC_4 <- cbind(Data_1, PCscores)


ggplot(data_PC_4, aes(PC1, PC2, color=Classification_name))+
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



#Saving data with classification:
Classification <- Data_3[,c(6:10)] 
setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Enviroment")
write.csv(Data_3, file = "Data_traits.csv")


colnames(Data_3)
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
  geom_point(data=Data_3, aes(x=Long, y=Lat, color=PC3), alpha = 7/10, size=2) +
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
    scale_color_gradientn(colours = rainbow(7))



a















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
