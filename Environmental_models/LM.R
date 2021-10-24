setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Enviroment")
Accessions_data <- read.table(file="Data_traits_short.csv", header=T, sep=",")
Enviromental_data <- read.table(file="shiny climatesd short.csv", header=T, sep=",")

colnames(Accessions_data)

#Quarter:
#WorldClim BIO16
#CHELSA BIO11
#Month:
#WorldClim BIO13
#WorldClim BIO6


Accessions <- Accessions_data[,c(1,4,5,10,11,13,15:20,27:30)]
Enviromental <-Enviromental_data[,c(1,2,42,81,88,91,115)]

Data_1 <- merge(Accessions, Enviromental, by="CS")
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

Data_1$GEO_1 <- NA
Data_1$GEO_1[which(Data_1$Country == "ESP")] <- "Iberian_Peninsula"
Data_1$GEO_1[which(Data_1$Country == "POR")] <- "Iberian_Peninsula"
Data_1$GEO_1[is.na(Data_1$GEO_1)] <- "Europe"

#Manovas:
Data_1$MAM_Status<- as.numeric(as.character(Data_1$MAM_Status))
Data_1$GSOH_Status<- as.numeric(as.character(Data_1$GSOH_Status))
Data_1$AOP_Status<- as.numeric(as.character(Data_1$AOP_Status))

Iberian_data <- Data_1[ which(Data_1$GEO_1=='Iberian_Peninsula'),]
Europe_data <- Data_1[ which(Data_1$GEO_1=='Europe'),]
Iberian_data <- droplevels(Iberian_data)
Europe_data <- droplevels(Europe_data)

#Grouping by geography:
Data_1$GEO <- NA
Data_1$GEO[which(Data_1$Country == "ESP")] <- "Iberian_Peninsula"
Data_1$GEO[which(Data_1$Country == "POR")] <- "Iberian_Peninsula"
Data_1$GEO[which(Data_1$Country == "ITA")] <- "Italy_Balkan"
Data_1$GEO[which(Data_1$Country == "BUL")] <- "Italy_Balkan"
Data_1$GEO[which(Data_1$Country == "CRO")] <- "Italy_Balkan"
Data_1$GEO[which(Data_1$Country == "GRC")] <- "Italy_Balkan"
Data_1$GEO[which(Data_1$Country == "SRB")] <- "Italy_Balkan"
Data_1$GEO[which(Data_1$Country == "ROU")] <- "Italy_Balkan"
Data_1$GEO[which(Data_1$Country == "LBN")] <- "Italy_Balkan"
Data_1$GEO[which(Data_1$Country == "FRA")] <- "France"
Data_1$GEO[which(Data_1$Country == "UK")] <- "UK"
Data_1$GEO[which(Data_1$Country == "IRL")] <- "UK"
Data_1$GEO[is.na(Data_1$GEO)] <- "Germany_and_East"

Iberian <- Data_1[ which(Data_1$GEO=='Iberian_Peninsula'),]
Iberian <- droplevels(Iberian)
Italy <- Data_1[ which(Data_1$GEO=='Italy_Balkan'),]
Italy <- droplevels(Italy)
France <- Data_1[ which(Data_1$GEO=='France'),]
France <- droplevels(France)
UK <- Data_1[ which(Data_1$GEO=='UK'),]
UK <- droplevels(UK)
Germany <- Data_1[ which(Data_1$GEO=='Germany_and_East'),]
Germany <- droplevels(Germany)

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
  scale_color_manual(values = c("#1f78b4", "#e31a1c", "#fb9a99", "#33a02c", "#fdbf6f", "#ff7f00"))



#All
#MAM, GSOH, AOP:

Ma_Quarter <- manova(cbind(MAM_Status, GSOH_Status, AOP_Status) ~ group + GEO+ WC2_BIO16 + CHELSA_BIO11 + SRTM_elevation+
                       group*GEO + WC2_BIO16*CHELSA_BIO11*SRTM_elevation +group*WC2_BIO16*CHELSA_BIO11*SRTM_elevation +GEO*WC2_BIO16*CHELSA_BIO11*SRTM_elevation, data = Data_1)
summary(Ma_Quarter)
summary.aov(Ma_Quarter)
cld(emmeans(Ma_Quarter, ~GEO_1))

Ma_Month <- manova(cbind(MAM_Status, GSOH_Status, AOP_Status) ~ group + GEO+ WC2_BIO6 + WC2_BIO13 + SRTM_elevation+
                     group*GEO +WC2_BIO6*WC2_BIO13*SRTM_elevation +group*WC2_BIO6*WC2_BIO13*SRTM_elevation +GEO*WC2_BIO6*WC2_BIO13*SRTM_elevation, data = Data_1)
summary(Ma_Month, tol=0)
summary.aov(Ma_Month)
cld(emmeans(Ma_Month, ~group))

count(Europe_data$Classification_name)
#Interaction between MAM and AOP:

Ma_Quarter_Int <- manova(cbind(MAM_Status, AOP_Status, MAM_Status*AOP_Status) ~ group + GEO+ WC2_BIO16 + CHELSA_BIO11 + SRTM_elevation+
                           group*GEO + WC2_BIO16*CHELSA_BIO11*SRTM_elevation +group*WC2_BIO16*CHELSA_BIO11*SRTM_elevation +GEO*WC2_BIO16*CHELSA_BIO11*SRTM_elevation, data = Data_1)
summary(Ma_Quarter_Int, tol=0)
summary.aov(Ma_Quarter_Int)
cld(emmeans(Ma_Quarter, ~GEO_1))

Ma_Month_Int <- manova(cbind(MAM_Status, AOP_Status, MAM_Status*AOP_Status) ~ group + GEO+ WC2_BIO6 + WC2_BIO13 + SRTM_elevation+
                         group*GEO +WC2_BIO6*WC2_BIO13*SRTM_elevation +group*WC2_BIO6*WC2_BIO13*SRTM_elevation +GEO*WC2_BIO6*WC2_BIO13*SRTM_elevation, data = Data_1)
summary(Ma_Month_Int, tol=0)
summary.aov(Ma_Month_Int)
cld(emmeans(Ma_Month, ~group))

anova(Ma_Quarter, Ma_Quarter_Int)
anova(Ma_Month, Ma_Month_Int)

#MAM at the left, AOP at the right: 
A_Quarter <- lm(MAM_Status ~ group + GEO+ WC2_BIO16 + CHELSA_BIO11 + SRTM_elevation+ AOP_Status+
                       +group*GEO*WC2_BIO16*CHELSA_BIO11*SRTM_elevation*AOP_Status , data = Data_1)
anova(A_Quarter)
slp <-  coef(A_Quarter)[2]
cld(posthoc[[A_Quarter]])
cld(emmeans(A_Quarter, ~group))
A_Quarter <- lm(MAM_Status ~ group + GEO+ WC2_BIO16 + CHELSA_BIO11 + SRTM_elevation+ AOP_Status
                , data = Data_1)
summary(A_Quarter)

A_Month <- lm(MAM_Status ~ group + GEO+ WC2_BIO6 + WC2_BIO13 + SRTM_elevation+ AOP_Status+
                  +group*GEO*WC2_BIO6*WC2_BIO13*SRTM_elevation*AOP_Status , data = Data_1)
anova(A_Month)
slp <-  coef(A_Month)[2]
cld(emmeans(A_Month, ~group))
A_Month <- lm(MAM_Status ~ group + GEO+ WC2_BIO6 + WC2_BIO13 + SRTM_elevation+ AOP_Status
               , data = Data_1)
summary(A_Month)

#Plot by Temp:
ggplot(data = Data_1, aes(x = MAM_Status, y = CHELSA_BIO11, color=GEO_1))+
  theme_minimal() + 
  geom_smooth(method = "lm")+
  geom_jitter(position = position_jitter(width = 0.1), alpha = 0.6)+
  scale_color_manual(values = c("black", "red"))+
  ylab("CHELSA BIO11 
       Mean temperature of coldest quarter") +
  xlab("")+
    scale_x_continuous(breaks = c(0, 1),
                     labels = c("C3", "C4"))+
  theme(plot.margin = unit(c(1, 8, 6, 2), "cm"))


ggplot(data = Data_1, aes(x = MAM_Status, y = WC2_BIO6, color=GEO_1))+
  theme_minimal() + 
  geom_smooth(method = "lm")+
  geom_jitter(position = position_jitter(width = 0.1), alpha = 0.6)+
  scale_color_manual(values = c("black", "red"))+
  ylab("WorldClim BIO6 
       Min temperature of coldest month") +
  xlab("")+
  scale_x_continuous(breaks = c(0, 1),
                     labels = c("C3", "C4"))+
  theme(plot.margin = unit(c(1, 8, 6, 2), "cm"))

ggplot(data = Data_1, aes(x = MAM_Status, y = SRTM_elevation, color=GEO_1))+
  theme_minimal() + 
  geom_smooth(method = "lm")+
  geom_jitter(position = position_jitter(width = 0.1), alpha = 0.6)+
  scale_color_manual(values = c("black", "red"))+
  ylab("SRTM_elevation") +
  xlab("MAM")+
  scale_x_continuous(breaks = c(0, 1),
                     labels = c("C3", "C4"))+
  theme(plot.margin = unit(c(1, 8, 6, 2), "cm"))

ggplot(data = Data_1, aes(x = MAM_Status, y = WC2_BIO13, color=GEO_1))+
  theme_minimal() + 
  geom_smooth(method = "lm")+
  geom_jitter(position = position_jitter(width = 0.1), alpha = 0.6)+
  scale_color_manual(values = c("black", "red"))+
  ylab("WorldClim BIO13:
Precipitation of wettest month") +
  xlab("MAM")+
  scale_x_continuous(breaks = c(0, 1),
                     labels = c("C3", "C4"))+
  theme(plot.margin = unit(c(1, 8, 6, 2), "cm"))
ylim(0,600)


#Iberian: 
#MAM, GSOH, AOP:
Ma_Quarter_I <- manova(cbind(MAM_Status, GSOH_Status, AOP_Status) ~ group + WC2_BIO16 + CHELSA_BIO11 + SRTM_elevation+
                         WC2_BIO16*CHELSA_BIO11*SRTM_elevation +group*WC2_BIO16*CHELSA_BIO11*SRTM_elevation , data = Iberian_data)
summary(Ma_Quarter_I,tol=0)
summary.aov(Ma_Quarter_I)
cld(emmeans(Ma_Quarter_I, ~group))


Ma_Month_I <- manova(cbind(MAM_Status, GSOH_Status, AOP_Status) ~ group + WC2_BIO6 + WC2_BIO13 + SRTM_elevation+
                       WC2_BIO6*WC2_BIO13*SRTM_elevation +group*WC2_BIO6*WC2_BIO13*SRTM_elevation , data = Iberian_data)
summary(Ma_Month_I, tol=0)
summary.aov(Ma_Month_I)
cld(emmeans(Ma_Month_I, ~group))

#MAM AOP interaction:
#Iberia:
Ma_Quarter_I <- manova(cbind(MAM_Status, AOP_Status, MAM_Status*AOP_Status) ~ group + WC2_BIO16 + CHELSA_BIO11 + SRTM_elevation+
                         WC2_BIO16*CHELSA_BIO11*SRTM_elevation +group*WC2_BIO16*CHELSA_BIO11*SRTM_elevation , data = Iberian_data)
summary(Ma_Quarter_I,tol=0)
summary.aov(Ma_Quarter_I)
cld(emmeans(Ma_Quarter_I, ~group))

Ma_Month_I <- manova(cbind(MAM_Status, AOP_Status, MAM_Status*AOP_Status) ~ group + WC2_BIO6 + WC2_BIO13 + SRTM_elevation+
                       WC2_BIO6*WC2_BIO13*SRTM_elevation +group*WC2_BIO6*WC2_BIO13*SRTM_elevation , data = Iberian_data)
summary(Ma_Month_I, tol=0)
summary.aov(Ma_Month_I)
cld(emmeans(Ma_Month_I, ~group))



slp <-  coef(Ma_Month_I)[2]


#MAM at the left, AOP at the right: 
A_Quarter <- lm(MAM_Status ~ group + WC2_BIO16 + CHELSA_BIO11 + SRTM_elevation+ AOP_Status+
                  +group*WC2_BIO16*CHELSA_BIO11*SRTM_elevation*AOP_Status , data = Iberian_data)
anova(A_Quarter)
slp <-  coef(A_Quarter)[2]
A_Quarter <- lm(MAM_Status ~ group + WC2_BIO16 + CHELSA_BIO11 + SRTM_elevation+ AOP_Status
                , data = Iberian_data)
summary(A_Quarter)

A_Month <- lm(MAM_Status ~ group + WC2_BIO6 + WC2_BIO13 + SRTM_elevation+ AOP_Status+
                +group*WC2_BIO6*WC2_BIO13*SRTM_elevation*AOP_Status , data = Iberian_data)
anova(A_Month)
slp <-  coef(A_Month)[2]
A_Month <- lm(MAM_Status ~ group + WC2_BIO6 + WC2_BIO13 + SRTM_elevation+ AOP_Status
              , data = Iberian_data)
summary(A_Month)

#Europ:
Ma_Quarter_E <- manova(cbind(MAM_Status, AOP_Status, MAM_Status*AOP_Status) ~ group + GEO+ WC2_BIO16 + CHELSA_BIO11 + SRTM_elevation+
                      GEO+group+ WC2_BIO16*CHELSA_BIO11*SRTM_elevation +group*WC2_BIO16*CHELSA_BIO11*SRTM_elevation 
                       +GEO*WC2_BIO16*CHELSA_BIO11*SRTM_elevation, data = Europe_data)
summary(Ma_Quarter_E,tol=0)
summary.aov(Ma_Quarter_E)
cld(emmeans(Ma_Quarter_E, ~group))

Ma_Month_E <- manova(cbind(MAM_Status, AOP_Status, MAM_Status*AOP_Status) ~ group + GEO +WC2_BIO6 + WC2_BIO13 + SRTM_elevation+
                    GEO*group+ WC2_BIO6*WC2_BIO13*SRTM_elevation +group*WC2_BIO6*WC2_BIO13*SRTM_elevation
                     +GEO*WC2_BIO6*WC2_BIO13*SRTM_elevation, data = Europe_data)
summary(Ma_Month_E, tol=0)
summary.aov(Ma_Month_E)
cld(emmeans(Ma_Month_E, ~group))

slp <-  coef(Ma_Month_E)[2]

tbl = table(Data_1$Classification_name, Europe_data$Classification_name) 
tbl
chisq.test(tbl)
assocstats(tbl)

colnames(Europe_data)
treemap(Europe_data, index="Classification_name", type="value", vSize ="WC2_BIO13")
treemap(Iberian_data, index="Classification_name", type="value", vSize ="WC2_BIO13")

#MAM at the left, AOP at the right: 
A_Quarter <- lm(MAM_Status ~ group +WC2_BIO16 + CHELSA_BIO11 + SRTM_elevation+ AOP_Status+
                  +group*WC2_BIO16*CHELSA_BIO11*SRTM_elevation*AOP_Status , data = Europe_data)

anova(A_Quarter)
slp <-  coef(A_Quarter)[2]
A_Quarter <- lm(MAM_Status ~ group +WC2_BIO16 + CHELSA_BIO11 + SRTM_elevation+ AOP_Status
                   , data = Europe_data)
summary(A_Quarter)

A_Month <- lm(MAM_Status ~ group + WC2_BIO6 + WC2_BIO13 + SRTM_elevation+ AOP_Status+
                +group*WC2_BIO6*WC2_BIO13*SRTM_elevation*AOP_Status , data = Europe_data)
anova(A_Month)
slp <-  coef(A_Month)[2]
A_Month <- lm(MAM_Status ~ group + WC2_BIO6 + WC2_BIO13 + SRTM_elevation+ AOP_Status
                 , data = Europe_data)
summary(A_Month)



#Other models:

Ma_Quarter_E <- manova(cbind(MAM_Status, GSOH_Status, AOP_Status) ~ group + WC2_BIO16 + CHELSA_BIO11 + SRTM_elevation+
                         WC2_BIO16*CHELSA_BIO11*SRTM_elevation +group*WC2_BIO16*CHELSA_BIO11*SRTM_elevation , data = Europe_data)
summary(Ma_Quarter_E,tol=0)
summary.aov(Ma_Quarter_E)
cld(emmeans(Ma_Quarter_E, ~group))

Ma_Month_E <- manova(cbind(MAM_Status, GSOH_Status, AOP_Status) ~ group + WC2_BIO6 + WC2_BIO13 + SRTM_elevation+
                       WC2_BIO6*WC2_BIO13*SRTM_elevation +group*WC2_BIO6*WC2_BIO13*SRTM_elevation , data = Europe_data)
summary(Ma_Month_E, tol=0)
summary.aov(Ma_Month_E)
cld(emmeans(Ma_Month_E, ~group))



  
#Grouping by geography:
Data_1$GEO <- NA
Data_1$GEO[which(Data_1$Country == "ESP")] <- "Iberian_Peninsula"
Data_1$GEO[which(Data_1$Country == "POR")] <- "Iberian_Peninsula"
Data_1$GEO[which(Data_1$Country == "ITA")] <- "Italy_Balkan"
Data_1$GEO[which(Data_1$Country == "BUL")] <- "Italy_Balkan"
Data_1$GEO[which(Data_1$Country == "CRO")] <- "Italy_Balkan"
Data_1$GEO[which(Data_1$Country == "GRC")] <- "Italy_Balkan"
Data_1$GEO[which(Data_1$Country == "SRB")] <- "Italy_Balkan"
Data_1$GEO[which(Data_1$Country == "ROU")] <- "Italy_Balkan"
Data_1$GEO[which(Data_1$Country == "LBN")] <- "Italy_Balkan"
Data_1$GEO[which(Data_1$Country == "FRA")] <- "France"
Data_1$GEO[which(Data_1$Country == "UK")] <- "UK"
Data_1$GEO[which(Data_1$Country == "IRL")] <- "UK"
Data_1$GEO[is.na(Data_1$GEO)] <- "Germany_and_East"

#Manovas:

Ma_Quarter <- manova(cbind(MAM_Status, GSOH_Status, AOP_Status) ~ group + GEO+ WC2_BIO16 + CHELSA_BIO11 + SRTM_elevation+
                       WC2_BIO16*CHELSA_BIO11*SRTM_elevation +group*WC2_BIO16*CHELSA_BIO11*SRTM_elevation +GEO*WC2_BIO16*CHELSA_BIO11*SRTM_elevation, data = Data_1)
summary(Ma_Quarter)
summary.aov(Ma_Quarter)
cld(emmeans(Ma_Quarter, ~GEO_1))

Ma_Month <- manova(cbind(MAM_Status, GSOH_Status, AOP_Status) ~ group + GEO+ WC2_BIO6 + WC2_BIO13 + SRTM_elevation+
                     WC2_BIO6*WC2_BIO13*SRTM_elevation +group*WC2_BIO6*WC2_BIO13*SRTM_elevation +GEO*WC2_BIO6*WC2_BIO13*SRTM_elevation, data = Data_1)
summary(Ma_Month, tol=0)
summary.aov(Ma_Month)
cld(emmeans(Ma_Month, ~group))

Iberian <- Data_1[ which(Data_1$GEO=='Iberian_Peninsula'),]
Iberian <- droplevels(Iberian)
Italy <- Data_1[ which(Data_1$GEO=='Italy_Balkan'),]
Italy <- droplevels(Italy)
France <- Data_1[ which(Data_1$GEO=='France'),]
France <- droplevels(France)
UK <- Data_1[ which(Data_1$GEO=='UK'),]
UK <- droplevels(UK)
Germany <- Data_1[ which(Data_1$GEO=='Germany_and_East'),]
Germany <- droplevels(Germany)

Ma_Quarter <- manova(cbind(MAM_Status, GSOH_Status, AOP_Status) ~ group + WC2_BIO16 + CHELSA_BIO11 + SRTM_elevation+
                       WC2_BIO16*CHELSA_BIO11*SRTM_elevation +group*WC2_BIO16*CHELSA_BIO11*SRTM_elevation, data = Iberian)
summary(Ma_Quarter)
summary.aov(Ma_Quarter)
cld(emmeans(Ma_Quarter, ~GEO_1))

Ma_Month <- manova(cbind(MAM_Status, GSOH_Status, AOP_Status) ~ group + WC2_BIO6 + WC2_BIO13 + SRTM_elevation+
                     WC2_BIO6*WC2_BIO13*SRTM_elevation +group*WC2_BIO6*WC2_BIO13*SRTM_elevation , data = Iberian)
summary(Ma_Month, tol=0)
summary.aov(Ma_Month)
cld(emmeans(Ma_Month, ~group))

Ma_Quarter <- manova(cbind(MAM_Status, GSOH_Status, AOP_Status) ~ group + WC2_BIO16 + CHELSA_BIO11 + SRTM_elevation+
                       WC2_BIO16*CHELSA_BIO11*SRTM_elevation +group*WC2_BIO16*CHELSA_BIO11*SRTM_elevation, data = UK)
summary(Ma_Quarter)
summary.aov(Ma_Quarter)
cld(emmeans(Ma_Quarter, ~GEO_1))

Ma_Month <- manova(cbind(MAM_Status, GSOH_Status, AOP_Status) ~ group + WC2_BIO6 + WC2_BIO13 + SRTM_elevation+
                     WC2_BIO6*WC2_BIO13*SRTM_elevation +group*WC2_BIO6*WC2_BIO13*SRTM_elevation , data = UK)
summary(Ma_Month, tol=0)
summary.aov(Ma_Month)
cld(emmeans(Ma_Month, ~group))

Ma_Quarter <- manova(cbind(MAM_Status, GSOH_Status, AOP_Status) ~ group + WC2_BIO16 + CHELSA_BIO11 + SRTM_elevation+
                       WC2_BIO16*CHELSA_BIO11*SRTM_elevation +group*WC2_BIO16*CHELSA_BIO11*SRTM_elevation, data = Germany)
summary(Ma_Quarter)
summary.aov(Ma_Quarter)
cld(emmeans(Ma_Quarter, ~GEO_1))

Ma_Month <- manova(cbind(MAM_Status, GSOH_Status, AOP_Status) ~ group + WC2_BIO6 + WC2_BIO13 + SRTM_elevation+
                     WC2_BIO6*WC2_BIO13*SRTM_elevation +group*WC2_BIO6*WC2_BIO13*SRTM_elevation , data = Germany)
summary(Ma_Month, tol=0)
summary.aov(Ma_Month)
cld(emmeans(Ma_Month, ~group))

Ma_Quarter <- manova(cbind(MAM_Status, GSOH_Status, AOP_Status) ~ group + WC2_BIO16 + CHELSA_BIO11 + SRTM_elevation+
                       WC2_BIO16*CHELSA_BIO11*SRTM_elevation +group*WC2_BIO16*CHELSA_BIO11*SRTM_elevation, data = Italy)
summary(Ma_Quarter)
summary.aov(Ma_Quarter)
cld(emmeans(Ma_Quarter, ~GEO_1))

Ma_Month <- manova(cbind(MAM_Status, GSOH_Status, AOP_Status) ~ group + WC2_BIO6 + WC2_BIO13 + SRTM_elevation+
                     WC2_BIO6*WC2_BIO13*SRTM_elevation +group*WC2_BIO6*WC2_BIO13*SRTM_elevation , data = Italy)
summary(Ma_Month, tol=0)
summary.aov(Ma_Month)
cld(emmeans(Ma_Month, ~group))

Ma_Quarter <- manova(cbind(MAM_Status, GSOH_Status, AOP_Status) ~ group + WC2_BIO16 + CHELSA_BIO11 + SRTM_elevation+
                       WC2_BIO16*CHELSA_BIO11*SRTM_elevation +group*WC2_BIO16*CHELSA_BIO11*SRTM_elevation, data = France)
summary(Ma_Quarter)
summary.aov(Ma_Quarter)
cld(emmeans(Ma_Quarter, ~GEO_1))

Ma_Month <- manova(cbind(MAM_Status, GSOH_Status, AOP_Status) ~ group + WC2_BIO6 + WC2_BIO13 + SRTM_elevation+
                     WC2_BIO6*WC2_BIO13*SRTM_elevation +group*WC2_BIO6*WC2_BIO13*SRTM_elevation , data = France)
summary(Ma_Month, tol=0)
summary.aov(Ma_Month)
cld(emmeans(Ma_Month, ~group))

#Everything but Iberia:
Ma_Quarter_E <- manova(cbind(MAM_Status, GSOH_Status, AOP_Status) ~ group + GEO + WC2_BIO16 + CHELSA_BIO11 + SRTM_elevation+
                         WC2_BIO16*CHELSA_BIO11*SRTM_elevation +group*WC2_BIO16*CHELSA_BIO11*SRTM_elevation + GEO*WC2_BIO16*CHELSA_BIO11*SRTM_elevation , data = Europe_data)
summary(Ma_Quarter_E,tol=0)
summary.aov(Ma_Quarter_E)
cld(emmeans(Ma_Quarter_E, ~group))

Ma_Month_E <- manova(cbind(MAM_Status, GSOH_Status, AOP_Status) ~ group + GEO + WC2_BIO6 + WC2_BIO13 + SRTM_elevation+
                       WC2_BIO6*WC2_BIO13*SRTM_elevation +group*WC2_BIO6*WC2_BIO13*SRTM_elevation +GEO*WC2_BIO6*WC2_BIO13*SRTM_elevation , data = Europe_data)
summary(Ma_Month_E, tol=0)
summary.aov(Ma_Month_E)
cld(emmeans(Ma_Month_E, ~group))


#Models:
#All data:
#Quarter:

lm_All_MAM_Quarter <- lm(data=Data, MAM_Status ~ group + GEO + WC2_BIO16 + CHELSA_BIO11 + SRTM_elevation+
                           WC2_BIO16*CHELSA_BIO11*SRTM_elevation +group*WC2_BIO16*CHELSA_BIO11*SRTM_elevation + GEO*WC2_BIO16*CHELSA_BIO11*SRTM_elevation)
anova(lm_All_MAM_Quarter)

Data_1<- Data[-c(39,72,78,124,167,222,242,276,337,430,431,474,513,581,596,640,679,695,728,786),]
lm_All_AOP_Quarter <- lm(data=Data_1, AOP_Status ~ group + GEO + WC2_BIO16 + CHELSA_BIO11 + SRTM_elevation+
                           WC2_BIO16*CHELSA_BIO11*SRTM_elevation +group*WC2_BIO16*CHELSA_BIO11*SRTM_elevation + GEO*WC2_BIO16*CHELSA_BIO11*SRTM_elevation)
anova(lm_All_AOP_Quarter)

lm_All_GSOH_Quarter <- lm(data=Data, GSOH_Status ~ group + GEO + WC2_BIO16 + CHELSA_BIO11 + SRTM_elevation+
                           WC2_BIO16*CHELSA_BIO11*SRTM_elevation +group*WC2_BIO16*CHELSA_BIO11*SRTM_elevation + GEO*WC2_BIO16*CHELSA_BIO11*SRTM_elevation)
anova(lm_All_GSOH_Quarter)

#Month:
lm_All_MAM_Month <- lm(data=Data, MAM_Status ~ group + GEO + WC2_BIO6 + WC2_BIO13 + SRTM_elevation+
                           WC2_BIO6*WC2_BIO13*SRTM_elevation +group*WC2_BIO6*WC2_BIO13*SRTM_elevation + GEO*WC2_BIO6*WC2_BIO13*SRTM_elevation)
anova(lm_All_MAM_Month)

lm_All_AOP_Month <- lm(data=Data_1, AOP_Status ~ group + GEO + WC2_BIO16 + WC2_BIO13 + SRTM_elevation+
                           WC2_BIO16*WC2_BIO13*SRTM_elevation +group*WC2_BIO16*WC2_BIO13*SRTM_elevation + GEO*WC2_BIO16*WC2_BIO13*SRTM_elevation)
anova(lm_All_AOP_Month)

lm_All_GSOH_Month <- lm(data=Data, GSOH_Status ~ group + GEO + WC2_BIO16 + WC2_BIO13 + SRTM_elevation+
                            WC2_BIO16*WC2_BIO13*SRTM_elevation +group*WC2_BIO16*WC2_BIO13*SRTM_elevation + GEO*WC2_BIO16*WC2_BIO13*SRTM_elevation)
anova(lm_All_GSOH_Month)


#Excluding Iberian from Europe:
Data$GEO_1 <- NA
Data$GEO_1[which(Data$Country == "ESP")] <- "Iberian_Peninsula"
Data$GEO_1[which(Data$Country == "POR")] <- "Iberian_Peninsula"
Data$GEO_1[which(Data$Country == "MAR")] <- "Iberian_Peninsula"
Data$GEO_1[is.na(Data$GEO_1)] <- "Europe"

#Quarter:

lm_All_MAM_Quarter <- lm(data=Data, MAM_Status ~ group + GEO_1 + WC2_BIO16 + CHELSA_BIO11 + SRTM_elevation+
                           WC2_BIO16*CHELSA_BIO11*SRTM_elevation +group*WC2_BIO16*CHELSA_BIO11*SRTM_elevation + GEO_1*WC2_BIO16*CHELSA_BIO11*SRTM_elevation)
anova(lm_All_MAM_Quarter)

Data_1<- Data[-c(39,72,78,124,167,222,242,276,337,430,431,474,513,581,596,640,679,695,728,786),]
lm_All_AOP_Quarter <- lm(data=Data_1, AOP_Status ~ group + GEO_1 + WC2_BIO16 + CHELSA_BIO11 + SRTM_elevation+
                           WC2_BIO16*CHELSA_BIO11*SRTM_elevation +group*WC2_BIO16*CHELSA_BIO11*SRTM_elevation + GEO_1*WC2_BIO16*CHELSA_BIO11*SRTM_elevation)
anova(lm_All_AOP_Quarter)

lm_All_GSOH_Quarter <- lm(data=Data, GSOH_Status ~ group + GEO_1 + WC2_BIO16 + CHELSA_BIO11 + SRTM_elevation+
                            WC2_BIO16*CHELSA_BIO11*SRTM_elevation +group*WC2_BIO16*CHELSA_BIO11*SRTM_elevation + GEO_1*WC2_BIO16*CHELSA_BIO11*SRTM_elevation)
anova(lm_All_GSOH_Quarter)

#Month:
lm_All_MAM_Month <- lm(data=Data, MAM_Status ~ group + GEO_1 + WC2_BIO6 + WC2_BIO13 + SRTM_elevation+
                         WC2_BIO6*WC2_BIO13*SRTM_elevation +group*WC2_BIO6*WC2_BIO13*SRTM_elevation + GEO_1*WC2_BIO6*WC2_BIO13*SRTM_elevation)
anova(lm_All_MAM_Month)

lm_All_AOP_Month <- lm(data=Data_1, AOP_Status ~ group + GEO_1 + WC2_BIO16 + WC2_BIO13 + SRTM_elevation+
                         WC2_BIO16*WC2_BIO13*SRTM_elevation +group*WC2_BIO16*WC2_BIO13*SRTM_elevation + GEO_1*WC2_BIO16*WC2_BIO13*SRTM_elevation)
anova(lm_All_AOP_Month)

lm_All_GSOH_Month <- lm(data=Data, GSOH_Status ~ group + GEO_1 + WC2_BIO16 + WC2_BIO13 + SRTM_elevation+
                          WC2_BIO16*WC2_BIO13*SRTM_elevation +group*WC2_BIO16*WC2_BIO13*SRTM_elevation + GEO_1*WC2_BIO16*WC2_BIO13*SRTM_elevation)
anova(lm_All_GSOH_Month)


#Only Iberian:
Iberian_data <- Data[ which(Data$GEO_1=='Iberian_Peninsula'),]
Europe_data <- Data[ which(Data$GEO_1=='Europe'),]

#Quarter:

lm_MAM_Quarter <- lm(data=Iberian_data, MAM_Status ~ group + WC2_BIO16 + CHELSA_BIO11 + SRTM_elevation+
                           WC2_BIO16*CHELSA_BIO11*SRTM_elevation +group*WC2_BIO16*CHELSA_BIO11*SRTM_elevation)
anova(lm_MAM_Quarter)


lm_AOP_Quarter <- lm(data=Iberian_data, AOP_Status ~ group + WC2_BIO16 + CHELSA_BIO11 + SRTM_elevation+
                           WC2_BIO16*CHELSA_BIO11*SRTM_elevation +group*WC2_BIO16*CHELSA_BIO11*SRTM_elevation )
anova(lm_AOP_Quarter)

lm_GSOH_Quarter <- lm(data=Iberian_data, GSOH_Status ~ group + WC2_BIO16 + CHELSA_BIO11 + SRTM_elevation+
                            WC2_BIO16*CHELSA_BIO11*SRTM_elevation +group*WC2_BIO16*CHELSA_BIO11*SRTM_elevation)
anova(lm_GSOH_Quarter)

#Month:
lm_MAM_Month <- lm(data=Iberian_data, MAM_Status ~ group + WC2_BIO6 + WC2_BIO13 + SRTM_elevation+
                         WC2_BIO6*WC2_BIO13*SRTM_elevation +group*WC2_BIO6*WC2_BIO13*SRTM_elevation)
anova(lm_MAM_Month)

lm_AOP_Month <- lm(data=Iberian_data, AOP_Status ~ group + WC2_BIO16 + WC2_BIO13 + SRTM_elevation+
                         WC2_BIO16*WC2_BIO13*SRTM_elevation +group*WC2_BIO16*WC2_BIO13*SRTM_elevation)
anova(lm_AOP_Month)

lm_GSOH_Month <- lm(data=Iberian_data, GSOH_Status ~ group + WC2_BIO16 + WC2_BIO13 + SRTM_elevation+
                          WC2_BIO16*WC2_BIO13*SRTM_elevation +group*WC2_BIO16*WC2_BIO13*SRTM_elevation)
anova(lm_GSOH_Month)

#Europe:
Eurpe_2<- Europe_data[-c(39,72,78,124,167,222,242,276,337,430,431,474,513,581,596,640,679,695,728,786),]  
Europe_1 <- na.omit(Europe_data) 
#Quarter:

lm_MAM_Quarter <- lm(data=Europe_data, MAM_Status ~ group + WC2_BIO16 + CHELSA_BIO11 + SRTM_elevation+
                       WC2_BIO16*CHELSA_BIO11*SRTM_elevation +group*WC2_BIO16*CHELSA_BIO11*SRTM_elevation)
anova(lm_MAM_Quarter)


#lm_AOP_Quarter <- lm(data=Europe_1, AOP_Status ~ group + WC2_BIO16 + CHELSA_BIO11 + SRTM_elevation+
                       WC2_BIO16*CHELSA_BIO11*SRTM_elevation +group*WC2_BIO16*CHELSA_BIO11*SRTM_elevation )
#anova(lm_AOP_Quarter)

lm_GSOH_Quarter <- lm(data=Europe_data, GSOH_Status ~ group + WC2_BIO16 + CHELSA_BIO11 + SRTM_elevation+
                        WC2_BIO16*CHELSA_BIO11*SRTM_elevation +group*WC2_BIO16*CHELSA_BIO11*SRTM_elevation)
anova(lm_GSOH_Quarter)

#Month:
lm_MAM_Month <- lm(data=Europe_data, MAM_Status ~ group + WC2_BIO6 + WC2_BIO13 + SRTM_elevation+
                     WC2_BIO6*WC2_BIO13*SRTM_elevation +group*WC2_BIO6*WC2_BIO13*SRTM_elevation)
anova(lm_MAM_Month)

#lm_AOP_Month <- lm(data=Iberian_data, AOP_Status ~ group + WC2_BIO16 + WC2_BIO13 + SRTM_elevation+
                     WC2_BIO16*WC2_BIO13*SRTM_elevation +group*WC2_BIO16*WC2_BIO13*SRTM_elevation)
#anova(lm_AOP_Month)

lm_GSOH_Month <- lm(data=Europe_data, GSOH_Status ~ group + WC2_BIO16 + WC2_BIO13 + SRTM_elevation+
                      WC2_BIO16*WC2_BIO13*SRTM_elevation +group*WC2_BIO16*WC2_BIO13*SRTM_elevation)
anova(lm_GSOH_Month)


#Manovas:
Data_1$MAM_Status<- as.numeric(as.character(Data_1$MAM_Status))
Data_1$GSOH_Status<- as.numeric(as.character(Data_1$GSOH_Status))
Data_1$AOP_Status<- as.numeric(as.character(Data_1$AOP_Status))

Iberian_data <- Data[ which(Data_1$GEO_1=='Iberian_Peninsula'),]
Europe_data <- Data[ which(Data_1$GEO_1=='Europe'),]

Europe_data$MAM_Status<- as.numeric(as.character(Europe_data$MAM_Status))
Europe_data$GSOH_Status<- as.numeric(as.character(Europe_data$GSOH_Status))
Europe_data$AOP_Status<- as.numeric(as.character(Europe_data$AOP_Status))

Iberian_data$MAM_Status<- as.numeric(as.character(Iberian_data$MAM_Status))
Iberian_data$GSOH_Status<- as.numeric(as.character(Iberian_data$GSOH_Status))
Iberian_data$AOP_Status<- as.numeric(as.character(Iberian_data$AOP_Status))


Ma_Quarter <- manova(cbind(MAM_Status, GSOH_Status, AOP_Status) ~ group + GEO_1+ WC2_BIO16 + CHELSA_BIO11 + SRTM_elevation+
                       WC2_BIO16*CHELSA_BIO11*SRTM_elevation +group*WC2_BIO16*CHELSA_BIO11*SRTM_elevation +GEO_1*WC2_BIO16*CHELSA_BIO11*SRTM_elevation, data = Data_1)
summary(Ma_Quarter)
summary.aov(Ma_Quarter)
cld(emmeans(Ma_Quarter, ~GEO_1))

Ma_Month <- manova(cbind(MAM_Status, GSOH_Status, AOP_Status) ~ group + GEO_1+ WC2_BIO6 + WC2_BIO13 + SRTM_elevation+
                       WC2_BIO6*WC2_BIO13*SRTM_elevation +group*WC2_BIO6*WC2_BIO13*SRTM_elevation +GEO_1*WC2_BIO6*WC2_BIO13*SRTM_elevation, data = Data_1)
summary(Ma_Month, tol=0)
summary.aov(Ma_Month)
cld(emmeans(Ma_Month, ~group))

Ma_Quarter <- manova(cbind(MAM_Status, GSOH_Status, AOP_Status) ~ group + GEO+ WC2_BIO16 + CHELSA_BIO11 + SRTM_elevation+
                       WC2_BIO16*CHELSA_BIO11*SRTM_elevation +group*WC2_BIO16*CHELSA_BIO11*SRTM_elevation +GEO*WC2_BIO16*CHELSA_BIO11*SRTM_elevation, data = Data_1)
summary(Ma_Quarter)
summary.aov(Ma_Quarter)
cld(emmeans(Ma_Quarter, ~GEO))

Ma_Month <- manova(cbind(MAM_Status, GSOH_Status, AOP_Status) ~ group + GEO+ WC2_BIO6 + WC2_BIO13 + SRTM_elevation+
                     WC2_BIO6*WC2_BIO13*SRTM_elevation +group*WC2_BIO6*WC2_BIO13*SRTM_elevation +GEO*WC2_BIO6*WC2_BIO13*SRTM_elevation, data = Data_1)
summary(Ma_Month, tol=0)
summary.aov(Ma_Month)
cld(emmeans(Ma_Month, ~GEO))


Ma_Quarter_I <- manova(cbind(MAM_Status, GSOH_Status, AOP_Status) ~ group + WC2_BIO16 + CHELSA_BIO11 + SRTM_elevation+
                       WC2_BIO16*CHELSA_BIO11*SRTM_elevation +group*WC2_BIO16*CHELSA_BIO11*SRTM_elevation , data = Iberian_data)
summary(Ma_Quarter_I,tol=0)
summary.aov(Ma_Quarter_I)
cld(emmeans(Ma_Quarter_I, ~group))

Ma_Month_I <- manova(cbind(MAM_Status, GSOH_Status, AOP_Status) ~ group + WC2_BIO6 + WC2_BIO13 + SRTM_elevation+
                     WC2_BIO6*WC2_BIO13*SRTM_elevation +group*WC2_BIO6*WC2_BIO13*SRTM_elevation , data = Iberian_data)
summary(Ma_Month_I, tol=0)
summary.aov(Ma_Month_I)
cld(emmeans(Ma_Month_I, ~group))

Ma_Quarter_E <- manova(cbind(MAM_Status, GSOH_Status, AOP_Status) ~ group + WC2_BIO16 + CHELSA_BIO11 + SRTM_elevation+
                         WC2_BIO16*CHELSA_BIO11*SRTM_elevation +group*WC2_BIO16*CHELSA_BIO11*SRTM_elevation , data = Europe_data)
summary(Ma_Quarter_E,tol=0)
summary.aov(Ma_Quarter_E)
cld(emmeans(Ma_Quarter_E, ~group))

Ma_Month_E <- manova(cbind(MAM_Status, GSOH_Status, AOP_Status) ~ group + WC2_BIO6 + WC2_BIO13 + SRTM_elevation+
                       WC2_BIO6*WC2_BIO13*SRTM_elevation +group*WC2_BIO6*WC2_BIO13*SRTM_elevation , data = Europe_data)
summary(Ma_Month_E, tol=0)
summary.aov(Ma_Month_E)
cld(emmeans(Ma_Month_E, ~group))



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
mymap1<-ggmap(get_googlemap(c(lon = -80, lat = 54),zoom=3, 
                            xlim=c(-10,72),
                            ylim=c(30, 80),
                            style=style_string), extent="device")

mymap2<-ggmap(get_googlemap(c(lon = 18, lat = 52),zoom=1, 
                            xlim=c(-10,72),
                            ylim=c(30, 80),
                            style=style_string), extent="device")

print(mymap2)+
  geom_point(data=Data_1, aes(x=Long, y=Lat, color=Classification_name), alpha = 7/10, size=1) +
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
