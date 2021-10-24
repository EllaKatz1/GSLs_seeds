#Here are two websites to design your maps:

#https://snazzymaps.com/editor
#https://mapstyle.withgoogle.com/


#First option, no need in API code:
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
  scale_color_manual(values = c("yellow", "blue", "grey", "orange", "red", "black", "green", "purple"))


ggsave(mymap, filename="mymap.pdf")


#A few maps options:

myMap1 <- get_stamenmap(bbox = c(left = 48,
                                 bottom = 62,
                                 right = -10,
                                 top = 40),
                        maptype = "terrain", 
                        crop = FALSE,
                        theme_bw(),
                        zoom = 5)
ggmap(myMap1)+ 
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
  scale_color_manual(values = c("yellow", "blue", "grey", "orange", "red", "black", "green", "purple"))


myMap2 <- get_stamenmap(bbox = c(left = 48,
                                 bottom = 62,
                                 right = -10,
                                 top = 40),
                        maptype = "watercolor", 
                        crop = FALSE,
                        zoom = 5)
ggmap(myMap2)+ 
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
  scale_color_manual(values = c("yellow", "blue", "grey", "orange", "red", "black", "green", "purple"))

#Second option:

basemap <- get_googlemap(c(lon = 18, lat = 52),zoom=4, 
                         xlim=c(-10,72),
                         ylim=c(30, 80),
                         #maptype = 'hybrid',
                         #maptype = 'terrain',
                         #maptype = 'satellite',
                         maptype = 'roadmap',
                         color="bw",
                         crop=FALSE,
                         key="AIzaSyAYVck1dRnEJY0Sfzsb9i5K9gWqlwExITI")
ggmap(basemap)+ 
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
  scale_color_manual(values = c("yellow", "blue", "grey", "orange", "red", "black", "green", "purple"))



basemap1 <- get_googlemap(c(lon = 18, lat = 52),zoom=4, 
                          xlim=c(-10,72),
                          ylim=c(30, 80),
                          #maptype = 'hybrid',
                          #maptype = 'terrain',
                          #maptype = 'satellite',
                          maptype = 'roadmap',
                          key="AIzaSyAYVck1dRnEJY0Sfzsb9i5K9gWqlwExITI")
ggmap(basemap1)+ 
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
  scale_color_manual(values = c("yellow", "blue", "grey", "orange", "red", "black", "green", "purple"))

ggmap(basemap1)+ 
  geom_point(data=Data_3, aes(x=Long, y=Lat, color=C3ratio_emmeans), alpha = 5/10) +
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
  theme(legend.title = element_text(colour="black", size=10, face="bold"))

