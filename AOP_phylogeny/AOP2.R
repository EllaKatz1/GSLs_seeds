BiocManager::install("Biostrings")
require(Biostrings)
install.packages("tidyverse")
library(tidyverse)
library(msa)
library(ape)
library(phangorn)
library(Biostrings)
library(ggtree)        
library(ggplot2)
install.packages("ggimage")
install.packages("ape")
install.packages("geiger")
install.packages("ape", dependencies = TRUE, INSTALL_opts = '--no-lock')
install.packages("geiger", dependencies = TRUE, INSTALL_opts = '--no-lock')
library(ggimage)
install.packages("ggrepel")
library(ggrepel)
install.packages("CRAN")
install.packages("dplyr")
library(dplyr)
install.packages("gheatmap")

library(tibble)

basedir<-("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny/AOP2 Chr41351568..1354216")

dna_AOP2 <- readDNAStringSet(file=file.path(basedir, "AOP2 clean and Lyrata and Kroymann.txt"), format = "fasta")
dna_AOP2
alignment_AOP2 <- msa(dna_AOP2)
phang_msa_AOP2 <- as.phyDat(alignment_AOP2, type = "dna")

dm_AOP2 <- dist.ml(phang_msa_AOP2)        

tree_AOP2 <- NJ(dm_AOP2)

ggtree(tree_AOP2)+ geom_tiplab()


setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny/AOP2 Chr41351568..1354216")  
info <- read.csv("Coords_all_K.csv")
cols <- c(South="deeppink2", North="chartreuse4", "OH-But"="red3", "3OHP"="green2", Allyl="blue", "3MSO"="yellow", "4MSO"="magenta", "4OHB"="skyblue", Butenyl="black", Lyrata="black",
          Sorbo="white", "Col-0"="white", "Cvi-0"="white", Unknown="black", "Ler-0"="white")

#Best two:
tree_AOP2_b<-root(tree_AOP2, "Lyrata")
plot(tree_AOP2_b)

AOP2 <- ggtree(tree_AOP2_b) %<+% info + 
  geom_tippoint(aes(color=Chemotype), size=2)+
  #geom_tiplab(linetype=NA,aes(color=Mountain), size=3) +
  geom_tiplab(linetype=NA, aes(label=Kroymann), size=3, hjust=1.3)+
  scale_color_manual(values=cols)+
  theme_tree2() + geom_rootedge()
AOP2


colnames(info)
#Heat maps:
#info_1<-info[,c(7,4)]
info_2<-info[,c(1,2,7)]
row.names(info_2) <- info_2$Accession
info_3<-info_2[,c(2:3)]
info_3$AOP <- NA

info_3$AOP[which(info_3$Chemotype == "3MSO")] <- "null"
info_3$AOP[which(info_3$Chemotype == "3OHP")] <- "AOP3"
info_3$AOP[which(info_3$Chemotype == "Allyl")] <- "AOP2"
info_3$AOP[which(info_3$Chemotype == "4MSO")] <- "null"
info_3$AOP[which(info_3$Chemotype == "4OHB")] <- "AOP3"
info_3$AOP[which(info_3$Chemotype == "Butenyl")] <- "AOP2"
info_3$AOP[which(info_3$Chemotype == "OH-But")] <- "AOP2"

info_3<-info_3[,c(1,3,2)]
colnames(info_3)


a<- ggtree(tree_AOP2_b) %<+% info + 
  geom_tippoint(aes(color=Chemotype), size=2) +
  scale_color_manual(values=cols)+ geom_rootedge(rootedge = .05)+
  geom_tiplab(linetype=NA, aes(label=Kroymann), size=3, hjust=-1.3)+
  theme_tree2()

a  

gheatmap(a, info_3, width=0.2, font.size=3, color=NULL, colnames_angle=-45, hjust=0)+
  scale_fill_manual(values=c(South="deeppink2", North="chartreuse4", "OH-But"="red3", "3OHP"="green2", Allyl="blue", "3MSO"="yellow", "4MSO"="magenta", "4OHB"="skyblue", Butenyl="black",
                             Lyrata="white", "Col-0"="white", "Sorbo"="white", "Cvi-0"="white", "Ler-0"="white", "NA"="white", AOP2="hotpink", AOP3="orange", null="green"))

ggsave("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny/MAM3/With all Kroymann/Tree with heat map.jpeg", dpi = 600)


#Rooting:
b<- ggtree(root(tree_MAM3, "Lyrata")) %<+% info + 
  geom_tippoint(aes(color=Chemotype), size=2) +
  scale_color_manual(values=cols)+ geom_rootedge(rootedge = .05)+
  geom_tiplab(linetype=NA, aes(label=Kroymann), size=3, align=T, hjust=1.5)+
  theme_tree2()

b  


#Tree to map:
install.packages("tidyverse")
library(tidyverse)  # data manipulation
install.packages("cluster")
library(cluster)    # clustering algorithms
install.packages("factoextra")
library(factoextra) # clustering visualization
install.packages("dendextend")
library(dendextend) # for comparing two dendrograms
install.packages("phytools")
library(phytools)
source('my_pylo2map.R') # I modified the code of this function a bit for my taste.

tree_AOP2_a<-root(tree_AOP2, "Lyrata")
phylo_AOP2 = as.phylo(tree_AOP2_a)


info$Color <- NA
info$Color[which(info$Chemotype == "OH-But")] <- "red3"
info$Color[which(info$Chemotype == "3OHP")] <- "green2"
info$Color[which(info$Chemotype == "4MSO")] <- "magenta"
info$Color[which(info$Chemotype == "Allyl")] <- "blue"
info$Color[which(info$Chemotype == "Lyrata")] <- "black"
info$Color[which(info$Chemotype == "4OHB")] <- "skyblue"
info$Color[which(info$Chemotype == "3MSO")] <- "yellow"
info$Color[which(info$Chemotype == "Butenyl")] <- "black"
info$Color[which(info$Chemotype == "Unknown")] <- "black"

colors_d<- info[,c(1,11)]

#Removing accessions that have no seq, Ler,Cvi, Col, Sorbo, Lyrata:
colors_d_1<-colors_d[c(-28, -787,-788,-789, -790, -114),]

colors_d_2 <- as.character(colors_d_1$Color)
names(colors_d_2) <- colors_d_1$Accession
view(colors_d_2)


plot(phylo_AOP2, length = 1)
#colors_b<- c("7000"="magenta", "6897"="red3", "7061"="red3", "7096"="green2", "7109"="red3", "8214"="red3", "7217"="magenta", "Lyrata"="black", "7273"="green2", 
#             "7296"="green2", "8357"="magenta", "7333"="green2", "7411"="blue")

coords_3 <- info[,c(1,5,6)]
#Removing accessions without seq:
coords_3_1<-coords_3[c(-28, -787,-788,-789, -790, -114),]

coords_4 <- coords_3_1[,-1]
rownames(coords_4) <- coords_3_1[,1]
coords_4 = as.matrix(coords_4)

#Droping the Lyrata, Col, Sorbo, Cvi, Ler:
tree_AOP2_b = drop.tip(tree_AOP2_a,'Lyrata')
tree_AOP2_b = drop.tip(tree_AOP2_b,'6909')
tree_AOP2_b = drop.tip(tree_AOP2_b,'6963')
tree_AOP2_b = drop.tip(tree_AOP2_b,'6911')
tree_AOP2_b = drop.tip(tree_AOP2_b,'7203')


phylo_b = as.phylo(tree_AOP2_b)
sort(phylo_b$edge.length)
phylo_b$edge.length = pmax(0,phylo_b$edge.length)  # some edges have negative lengths. Not sure what this is.
plot(phylo_b)



map = phylo.to.map(phylo_b ,coords_4, database='world', plot=F,xlim = c(-20, 59),ylim=c(35, 71))

map = phylo.to.map(phylo_b,coords_4, colors=colors_d_2, split = c(.3,.55), lwd=.3,lty=1,
                   cex.points=c(.8,1.5),  
                   mar=c(0,3,0,0), direction = 'downwards', 
                   database='world', plot=T,xlim = c(-20, 59),ylim=c(35, 71))

my_phylo2map(map,colors = colors_d_2,
             split = c(.3,.55), # relative vertical sizes of tree and map
             ftype='off',# turn off tree labels
             lwd=.3,lty=1, # size and style of lines in both map and connections
             cex.points=c(1,1), # size of leaf circles and map circles
             mar=c(0,3,0,0), # margin sizes (see par())
             main = 'AOP2',
             rotate=FALSE,
             direction = 'downwards' # alternative "rightwards" for tree to the left
) 
plot(tree_AOP2_b, use.edge.length = FALSE)

#Droping the NNNN accessions:
tree_AOP2_b = drop.tip(tree_AOP2_b,'9089')
tree_AOP2_b = drop.tip(tree_AOP2_b,'9099')
tree_AOP2_b = drop.tip(tree_AOP2_b,'9081')
tree_AOP2_b = drop.tip(tree_AOP2_b,'9085')
tree_AOP2_b = drop.tip(tree_AOP2_b,'9115')
tree_AOP2_b = drop.tip(tree_AOP2_b,'9084')
tree_AOP2_b = drop.tip(tree_AOP2_b,'9079')
tree_AOP2_b = drop.tip(tree_AOP2_b,'9759')
tree_AOP2_b = drop.tip(tree_AOP2_b,'9095')
tree_AOP2_b = drop.tip(tree_AOP2_b,'9114')
tree_AOP2_b = drop.tip(tree_AOP2_b,'9069')
tree_AOP2_b = drop.tip(tree_AOP2_b,'9070')
tree_AOP2_b = drop.tip(tree_AOP2_b,'9075')
tree_AOP2_b = drop.tip(tree_AOP2_b,'9078')
tree_AOP2_b = drop.tip(tree_AOP2_b,'9091')
tree_AOP2_b = drop.tip(tree_AOP2_b,'9111')
tree_AOP2_b = drop.tip(tree_AOP2_b,'10014')
tree_AOP2_b = drop.tip(tree_AOP2_b,'10013')

phylo_b = as.phylo(tree_AOP2_b)
sort(phylo_b$edge.length)
phylo_b$edge.length = pmax(0,phylo_b$edge.length)  # some edges have negative lengths. Not sure what this is.
plot(phylo_b)


AOP2a <- ggtree(tree_AOP2_b) %<+% info + 
  geom_tippoint(aes(color=Chemotype), size=2)+
  #geom_tiplab(linetype=NA,aes(color=Mountain), size=3) +
  geom_tiplab(linetype=NA, aes(label=Kroymann), size=3, hjust=1.3)+
  scale_color_manual(values=cols)+
  theme_tree2() + geom_rootedge()+xlim(0.06,0.11)
AOP2a


#Removing NNNNN accessions:
colors_d_3<-colors_d_1[c(-27, -478,-382,-428, -430, -412, -429, -427, -59, -381, -738, -425, -426, -479,-413, -26, -415, -737),]

colors_d_4 <- as.character(colors_d_3$Color)
names(colors_d_4) <- colors_d_3$Accession
view(colors_d_4)

#Removing NNNNN accessions:
coords_3_2<-coords_3_1[c(-27, -478,-382,-428, -430, -412, -429, -427, -59, -381, -738, -425, -426, -479,-413, -26, -415, -737),]

coords_5 <- coords_3_2[,-1]
rownames(coords_5) <- coords_3_2[,1]
coords_5 = as.matrix(coords_5)

map_a = phylo.to.map(phylo_b ,coords_5, database='world', plot=F,xlim = c(-20, 59),ylim=c(35, 71))

my_phylo2map(map_a,colors = colors_d_4,
             split = c(.3,.55), # relative vertical sizes of tree and map
             ftype='off',# turn off tree labels
             lwd=.3,lty=1, # size and style of lines in both map and connections
             cex.points=c(1,1), # size of leaf circles and map circles
             mar=c(0,3,0,0), # margin sizes (see par())
             main = 'AOP2',
             rotate=FALSE,
             direction = 'downwards' # alternative "rightwards" for tree to the left
) 


#Subseting the tree:

#By clades:

AOP2a+xlim(0.085,0.095)+ylim(80,120)+ geom_tiplab(linetype=1 ,aes(color=Chemotype), size=3)

#The clades: 
e<- ggtree(tree_AOP2_b) %<+% info + 
  geom_tippoint(aes(color=Chemotype), size=2) +
  scale_color_manual(values=cols)+ geom_rootedge(rootedge = .05)+
  geom_strip('9811', '7430', barsize=2, color='green2', 
             label="3OHP Clade", offset=-0.0095, fontsize=6) + 
   xlim(0.08,0.105)+
   theme_tree2()

e  

node_1<-fastMRCA(tree_AOP2_a,'9811', '7430')
pr.clade_1<-extract.clade(tree_AOP2_a,node_1)
plotTree(pr.clade_1,ftype="i")

write.csv(pr.clade_1$tip.label,"C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny/AOP2 Chr41351568..1354216/Clade_1.csv", row.names = FALSE)

setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny/AOP2 Chr41351568..1354216")  
Clade_1 <- read.csv("Clade_1.csv")

phylo_clade_1=as.phylo(pr.clade_1)

coords_clade_1<-merge(coords_3_1,Clade_1, by="Accession")

coords_clade_1a <- coords_clade_1[,-1]
rownames(coords_clade_1a) <- coords_clade_1[,1]
coords_clade_1a = as.matrix(coords_clade_1a)

colors_clade_1<-merge(colors_d, Clade_1, by="Accession")

colors_clade_1a <- as.character(colors_clade_1$Color)
names(colors_clade_1a ) <- colors_clade_1$Accession
view(colors_clade_1a)

map_clade_1 = phylo.to.map(phylo_clade_1,coords_clade_1a, 
                           database='world', plot=F, xlim = c(-20, 59),ylim=c(35, 71))


map_clade_1a = phylo.to.map(phylo_clade_1,coords_clade_1a, colors = colors_clade_1a,
                            lwd=.3,lty=1, cex.points=c(1.1,1.3),
                            database='world', plot=T, xlim = c(-20, 59),ylim=c(35, 71))

my_phylo2map(map_clade_1,colors = colors_clade_1a,
             split = c(.3,.55), # relative vertical sizes of tree and map
             ftype='off',# turn off tree labels
             lwd=.3,lty=1, # size and style of lines in both map and connections
             cex.points=c(1.1,1.3), # size of leaf circles and map circles
             mar=c(0,3,0,0), # margin sizes (see par())
             direction = 'downwards' # alternative "rightwards" for tree to the left
) 

basedir<-("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny/AOP2 Chr41351568..1354216")

dna_AOP2_null <- readDNAStringSet(file=file.path(basedir, "AOP2 null.txt"), format = "fasta")
dna_AOP2_null
alignment_AOP2_null <- msa(dna_AOP2_null)
phang_msa_AOP2_null <- as.phyDat(alignment_AOP2_null, type = "dna")
phang_msa_AOP2_null_a <- as.phyDat(alignment_AOP2_null, type = "USER",  levels = c("A", "G", "C", "T", "-"))


dm_AOP2_null_a <- dist.ml(phang_msa_AOP2_null_a)        

tree_AOP2_null_a <- NJ(dm_AOP2_null_a)

P<-ggtree(tree_AOP2_null)+ geom_tiplab()
P

setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny/AOP2 Chr41351568..1354216")  
info_null <- read.csv("Coords_null.csv")
cols <- c(South="deeppink2", North="chartreuse4", "OH-But"="red3", "3OHP"="green2", Allyl="blue", "3MSO"="yellow", "4MSO"="magenta", "4OHB"="skyblue", Butenyl="black", Lyrata="black",
          Sorbo="white", "Col-0"="white", "Cvi-0"="white", Unknown="black", "Ler-0"="white")


tree_AOP2_null_b<-root(tree_AOP2_null, "6911")
plot(tree_AOP2_null_b)

tree_AOP2_null_c<-root(tree_AOP2_null, "7213")
plot(tree_AOP2_null_c)

tree_AOP2_null_d<-root(tree_AOP2_null_a, "6911")
plot(tree_AOP2_null_d)


AOP2_null_b <- ggtree(tree_AOP2_null_b) %<+% info_null + 
  geom_tippoint(aes(color=Chemotype), size=2)+
  geom_tiplab(linetype=NA,aes(color=Mountain), size=3) +
  geom_tiplab(linetype=NA, aes(label=Kroymann), size=3, align = T)+
  scale_color_manual(values=cols)+
  theme_tree2() + geom_rootedge()
AOP2_null_b

AOP2_null_c <- ggtree(tree_AOP2_null_c) %<+% info_null + 
  geom_tippoint(aes(color=Chemotype), size=2)+
  geom_tiplab(linetype=NA,aes(color=Mountain), size=3) +
  geom_tiplab(linetype=NA, aes(label=Kroymann), size=3, hjust=-1)+
  scale_color_manual(values=cols)+
  theme_tree2() + geom_rootedge()
AOP2_null_c

AOP2_null_d <- ggtree(tree_AOP2_null_d) %<+% info_null + 
  geom_tippoint(aes(color=Chemotype), size=2)+
  geom_tiplab(linetype=NA,aes(color=Mountain), size=3) +
  geom_tiplab(linetype=NA, aes(label=Kroymann), size=3, align = T)+
  scale_color_manual(values=cols)+
  theme_tree2() + geom_rootedge()
AOP2_null_d


tree_AOP2_null_b = drop.tip(tree_AOP2_null_b,'6911')
tree_AOP2_null_b = drop.tip(tree_AOP2_null_b,'7213')
phylo_null_b = as.phylo(tree_AOP2_null_b)

#Colors list:
Colors_null<- merge(colors_d, info_null, by="Accession")
Colors_null_1<-Colors_null[c(-8,-75), c(1,2)]
Colors_null_2<-as.character(Colors_null_1$Color)
names(Colors_null_2)<-Colors_null_1$Accession
view(Colors_null_2)

#Coords list:
Coords_null<- Colors_null[c(-8,-75), c(1,6,7)]
Coords_null_1 <- Coords_null[,-1]
rownames(Coords_null_1) <- Coords_null[,1]
Coords_null_1 = as.matrix(Coords_null_1)

map_null = phylo.to.map(phylo_null_b ,Coords_null_1, database='world', plot=F,xlim = c(-20, 59),ylim=c(35, 71))

my_phylo2map(map_null,colors = Colors_null_2,
             split = c(.3,.55), # relative vertical sizes of tree and map
             ftype='off',# turn off tree labels
             lwd=.3,lty=1, # size and style of lines in both map and connections
             cex.points=c(1,1), # size of leaf circles and map circles
             mar=c(0,3,0,0), # margin sizes (see par())
             main = 'AOP2, null',
             rotate=FALSE,
             direction = 'downwards' # alternative "rightwards" for tree to the left
) 


install.packages("ggmsa")
library("ggmsa")

ggmsa(dna_AOP2_null, 1540,1610, color = "Chemistry_NT" )

data = tidy_msa(dna_AOP2_null, 750, 780)
AOP2_null_b + geom_facet(geom = geom_msa, data = data,  panel = 'msa',
                color = "Clustal") 


data = tidy_msa(dna_AOP2_null, 740, 800)
AOP2_null_b + geom_facet(geom = geom_msa, data = data,  panel = 'msa',
                         color = "Clustal") 


data = tidy_msa(dna_AOP2_null, 1580,1610)
AOP2_null_d + geom_facet(geom = geom_msa, data = data,  panel = 'msa',
                         color = "Clustal") 



basedir<-("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny/AOP2 Chr41351568..1354216")

dna_test <- readDNAStringSet(file=file.path(basedir, "ColLerCvi.txt"), format = "fasta")
dna_test
alignment_test <- msa(dna_test)
phang_msa_test <- as.phyDat(alignment_test, type = "USER",  levels = c("A", "G", "C", "T", "-"))


dm_test <- dist.ml(phang_msa_test)        

tree_test <- NJ(dm_test)

test<-ggtree(tree_test)+ geom_tiplab()
test

data = tidy_msa(dna_test, 101,150)
test + geom_facet(geom = geom_msa, data = data,  panel = 'msa',
                         color = "Clustal") 
ggmsa(dna_test, 1580,1630, font=NULL, color = "Chemistry_NT" ) 

data_test = tidy_msa(dna_test, 1570, 1610)
test + geom_facet(geom = geom_msa, data = data_test,  panel = 'msa',
                         color = "Clustal") 



node_2<-fastMRCA(tree_MAM3_a,'5486', '9883')
pr.clade_2<-extract.clade(tree_MAM3_a,node_2)
plotTree(pr.clade_2,ftype="i")

write.csv(pr.clade_2$tip.label,"C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny/MAM3/With all Kroymann/Clade_2.csv", row.names = FALSE)

setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny/MAM3/With all Kroymann")  
Clade_2 <- read.csv("Clade_2.csv")

phylo_clade_2=as.phylo(pr.clade_2)

coords_clade_2<-merge(coords_3,Clade_2, by="Accession")

coords_clade_2a <- coords_clade_2[,-1]
rownames(coords_clade_2a) <- coords_clade_2[,1]
coords_clade_2a = as.matrix(coords_clade_2a)

colors_clade_2<-merge(colors_d, Clade_2, by="Accession")

colors_clade_2a <- as.character(colors_clade_2$Color)
names(colors_clade_2a ) <- colors_clade_2$Accession
view(colors_clade_2a)


map_clade_2 = phylo.to.map(phylo_clade_2,coords_clade_2a, database='world', plot=F, xlim = c(-20, 59),ylim=c(35, 71))

my_phylo2map(map_clade_2,colors = colors_clade_2a,
             split = c(.3,.55), # relative vertical sizes of tree and map
             ftype='off',# turn off tree labels
             lwd=.3,lty=1, # size and style of lines in both map and connections
             cex.points=c(1.1,1.3), # size of leaf circles and map circles
             mar=c(0,3,0,0), # margin sizes (see par())
             direction = 'downwards' # alternative "rightwards" for tree to the left
) 


node_3<-fastMRCA(tree_MAM3_a,'7223', '9725')
pr.clade_3<-extract.clade(tree_MAM3_a,node_3)
plotTree(pr.clade_3,ftype="i")


write.csv(pr.clade_3$tip.label,"C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny/MAM3/With all Kroymann/Clade_3.csv", row.names = FALSE)

setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny/MAM3/With all Kroymann")  
Clade_3 <- read.csv("Clade_3.csv")
pr.clade_3 = drop.tip(pr.clade_3,'7236')
phylo_clade_3=as.phylo(pr.clade_3)

coords_clade_3<-merge(coords_3,Clade_3, by="Accession")

coords_clade_3a <- coords_clade_3[,-1]
rownames(coords_clade_3a) <- coords_clade_3[,1]
coords_clade_3a = as.matrix(coords_clade_3a)

colors_clade_3<-merge(colors_d, Clade_3, by="Accession")

colors_clade_3a <- as.character(colors_clade_3$Color)
names(colors_clade_3a ) <- colors_clade_3$Accession
view(colors_clade_3a)


map_clade_3 = phylo.to.map(phylo_clade_3,coords_clade_3a, database='world', plot=F, xlim = c(-20, 59),ylim=c(35, 71))

my_phylo2map(map_clade_3,colors = colors_clade_3a,
             split = c(.3,.55), # relative vertical sizes of tree and map
             ftype='off',# turn off tree labels
             lwd=.3,lty=1, # size and style of lines in both map and connections
             cex.points=c(1.1,1.3), # size of leaf circles and map circles
             mar=c(0,3,0,0), # margin sizes (see par())
             direction = 'downwards' # alternative "rightwards" for tree to the left
) 


node_4<-fastMRCA(tree_MAM3_a,'6968', '9593')
pr.clade_4<-extract.clade(tree_MAM3_a,node_4)
plotTree(pr.clade_4,ftype="i")


write.csv(pr.clade_4$tip.label,"C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny/MAM3/With all Kroymann/Clade_4.csv", row.names = FALSE)

setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny/MAM3/With all Kroymann")  
Clade_4 <- read.csv("Clade_4.csv")

phylo_clade_4=as.phylo(pr.clade_4)

coords_clade_4<-merge(coords_3,Clade_4, by="Accession")

coords_clade_4a <- coords_clade_4[,-1]
rownames(coords_clade_4a) <- coords_clade_4[,1]
coords_clade_4a = as.matrix(coords_clade_4a)

colors_clade_4<-merge(colors_d, Clade_4, by="Accession")

colors_clade_4a <- as.character(colors_clade_4$Color)
names(colors_clade_4a ) <- colors_clade_4$Accession


map_clade_4 = phylo.to.map(phylo_clade_4,coords_clade_4a, database='world', plot=F, xlim = c(-20, 59),ylim=c(35, 71))

my_phylo2map(map_clade_4,colors = colors_clade_4a,
             split = c(.3,.55), # relative vertical sizes of tree and map
             ftype='off',# turn off tree labels
             lwd=.3,lty=1, # size and style of lines in both map and connections
             cex.points=c(1.1,1.3), # size of leaf circles and map circles
             mar=c(0,3,0,0), # margin sizes (see par())
             direction = 'downwards' # alternative "rightwards" for tree to the left
) 


node_5<-fastMRCA(tree_MAM3_a,'9554', '6911')
pr.clade_5<-extract.clade(tree_MAM3_a,node_5)
plotTree(pr.clade_5,ftype="i")


write.csv(pr.clade_5$tip.label,"C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny/MAM3/With all Kroymann/Clade_5.csv", row.names = FALSE)

setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny/MAM3/With all Kroymann")  
Clade_5 <- read.csv("Clade_5.csv")

phylo_clade_5=as.phylo(pr.clade_5)

coords_clade_5<-merge(coords_3,Clade_5, by="Accession")

coords_clade_5a <- coords_clade_5[,-1]
rownames(coords_clade_5a) <- coords_clade_5[,1]
coords_clade_5a = as.matrix(coords_clade_5a)

colors_clade_5<-merge(colors_d, Clade_5, by="Accession")

colors_clade_5a <- as.character(colors_clade_5$Color)
names(colors_clade_5a ) <- colors_clade_5$Accession


map_clade_5 = phylo.to.map(phylo_clade_5,coords_clade_5a, database='world', plot=F, xlim = c(-20, 59),ylim=c(35, 71))

my_phylo2map(map_clade_5,colors = colors_clade_5a,
             split = c(.3,.55), # relative vertical sizes of tree and map
             ftype='off',# turn off tree labels
             lwd=.3,lty=1, # size and style of lines in both map and connections
             cex.points=c(1.1,1.3), # size of leaf circles and map circles
             mar=c(0,3,0,0), # margin sizes (see par())
             direction = 'downwards' # alternative "rightwards" for tree to the left
) 


node_6<-fastMRCA(tree_MAM3_a,'9902', '7164')
pr.clade_6<-extract.clade(tree_MAM3_a,node_6)
plotTree(pr.clade_6,ftype="i")


write.csv(pr.clade_6$tip.label,"C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny/MAM3/With all Kroymann/Clade_6.csv", row.names = FALSE)

setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny/MAM3/With all Kroymann")  
Clade_6 <- read.csv("Clade_6.csv")

phylo_clade_6=as.phylo(pr.clade_6)

coords_clade_6<-merge(coords_3,Clade_6, by="Accession")

coords_clade_6a <- coords_clade_6[,-1]
rownames(coords_clade_6a) <- coords_clade_6[,1]
coords_clade_6a = as.matrix(coords_clade_6a)

colors_clade_6<-merge(colors_d, Clade_6, by="Accession")

colors_clade_6a <- as.character(colors_clade_6$Color)
names(colors_clade_6a ) <- colors_clade_6$Accession


map_clade_6 = phylo.to.map(phylo_clade_6,coords_clade_6a, database='world', plot=F, xlim = c(-20, 59),ylim=c(35, 71))

my_phylo2map(map_clade_6,colors = colors_clade_6a,
             split = c(.3,.55), # relative vertical sizes of tree and map
             ftype='off',# turn off tree labels
             lwd=.3,lty=1, # size and style of lines in both map and connections
             cex.points=c(1.1,1.3), # size of leaf circles and map circles
             mar=c(0,3,0,0), # margin sizes (see par())
             direction = 'downwards' # alternative "rightwards" for tree to the left
) 

node_7<-fastMRCA(tree_MAM3_a,'9537', '9655')
pr.clade_7<-extract.clade(tree_MAM3_a,node_7)
plotTree(pr.clade_7,ftype="i")


write.csv(pr.clade_7$tip.label,"C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny/MAM3/With all Kroymann/Clade_7.csv", row.names = FALSE)

setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny/MAM3/With all Kroymann")  
Clade_7 <- read.csv("Clade_7.csv")

phylo_clade_7=as.phylo(pr.clade_7)

coords_clade_7<-merge(coords_3,Clade_7, by="Accession")

coords_clade_7a <- coords_clade_7[,-1]
rownames(coords_clade_7a) <- coords_clade_7[,1]
coords_clade_7a = as.matrix(coords_clade_7a)

colors_clade_7<-merge(colors_d, Clade_7, by="Accession")

colors_clade_7a <- as.character(colors_clade_7$Color)
names(colors_clade_7a ) <- colors_clade_7$Accession


map_clade_7 = phylo.to.map(phylo_clade_7,coords_clade_7a, database='world', plot=F, xlim = c(-20, 59),ylim=c(35, 71))

my_phylo2map(map_clade_7,colors = colors_clade_7a,
             split = c(.3,.55), # relative vertical sizes of tree and map
             ftype='off',# turn off tree labels
             lwd=.3,lty=1, # size and style of lines in both map and connections
             cex.points=c(1.1,1.3), # size of leaf circles and map circles
             mar=c(0,3,0,0), # margin sizes (see par())
             direction = 'downwards' # alternative "rightwards" for tree to the left
) 

node_8<-fastMRCA(tree_MAM3_a,'9085', '9910')
pr.clade_8<-extract.clade(tree_MAM3_a,node_8)
plotTree(pr.clade_8,ftype="i")


write.csv(pr.clade_8$tip.label,"C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny/MAM3/With all Kroymann/Clade_8.csv", row.names = FALSE)

setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny/MAM3/With all Kroymann")  
Clade_8 <- read.csv("Clade_8.csv")

phylo_clade_8=as.phylo(pr.clade_8)

coords_clade_8<-merge(coords_3,Clade_8, by="Accession")

coords_clade_8a <- coords_clade_8[,-1]
rownames(coords_clade_8a) <- coords_clade_8[,1]
coords_clade_8a = as.matrix(coords_clade_8a)

colors_clade_8<-merge(colors_d, Clade_8, by="Accession")

colors_clade_8a <- as.character(colors_clade_8$Color)
names(colors_clade_8a ) <- colors_clade_8$Accession


map_clade_8 = phylo.to.map(phylo_clade_8,coords_clade_8a, database='world', plot=F, xlim = c(-20, 59),ylim=c(35, 71))

my_phylo2map(map_clade_8,colors = colors_clade_8a,
             split = c(.3,.55), # relative vertical sizes of tree and map
             ftype='off',# turn off tree labels
             lwd=.3,lty=1, # size and style of lines in both map and connections
             cex.points=c(1.1,1.3), # size of leaf circles and map circles
             mar=c(0,3,0,0), # margin sizes (see par())
             direction = 'downwards' # alternative "rightwards" for tree to the left
) 






#Clades to map:
colors_clade_1b<-colors_clade_1
colors_clade_1b$Clade <- NA
colors_clade_1b$Clade <- "Clade_1"

colors_clade_2b<-colors_clade_2
colors_clade_2b$Clade <- NA
colors_clade_2b$Clade <- "Clade_2"

colors_clade_3b<-colors_clade_3
colors_clade_3b$Clade <- NA
colors_clade_3b$Clade <- "Clade_3"

colors_clade_4b<-colors_clade_4
colors_clade_4b$Clade <- NA
colors_clade_4b$Clade <- "Clade_4"

colors_clade_5b<-colors_clade_5
colors_clade_5b$Clade <- NA
colors_clade_5b$Clade <- "Clade_5"

colors_clade_6b<-colors_clade_6
colors_clade_6b$Clade <- NA
colors_clade_6b$Clade <- "Clade_6"

colors_clade_7b<-colors_clade_7
colors_clade_7b$Clade <- NA
colors_clade_7b$Clade <- "Clade_7"

colors_clade_8b<-colors_clade_8
colors_clade_8b$Clade <- NA
colors_clade_8b$Clade <- "Clade_8"

colors_clades<- rbind(colors_clade_1b, colors_clade_2b, colors_clade_3b, colors_clade_4b, colors_clade_5b, colors_clade_6b, colors_clade_7b, colors_clade_8b)
write.csv(colors_clades,"C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny/MAM3/With all Kroymann/colors_clades.csv", row.names = FALSE)


colors_clades_a<- merge(colors_clades, coords_3, by='Accession')
colnames(info)
Kroymann<-info[,c(1,9)]
colors_clades_a<- merge(colors_clades_a, Kroymann, by='Accession')


#Maps:
install.packages('ggmap')
library(ggmap)
install.packages('CRAN')
library("rjson")
library(maps)
install.packages("RJSONIO")
library(RJSONIO)
install.packages("googleway") 
library("googleway")

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

colnames(colors_clades_a)
print(mymap)+
  geom_point(data=colors_clades_a, aes(x=Long, y=Lat, color=Clade), alpha = 7/10, size=3) +
  ggtitle('') +
  xlab('Longitude') +
  ylab('Latitude') +
  theme(text = element_text(size=18)) +
  theme(axis.text=element_text(size=14,color='black'),
        axis.title=element_text(size=20,color='black')) +
  theme(plot.title = element_text(size = 26, vjust=1)) +
  theme(plot.title = element_text(size = 18,face="bold")) +
  theme(legend.key = element_rect(fill = "white")) +
  theme(legend.text = element_text(colour="black", size=16))+
  theme(legend.title = element_text(colour="black", size=14, face="bold"))+
  scale_color_manual(values = c("gold", "green", "red",  "violetred1", "gray44", "blue1",  "purple3", "black"))

print(mymap)+
  geom_point(data=colors_clades_a, aes(x=Long, y=Lat, color=Clade), alpha = 7/10, size=3) +
  ggtitle('') +
  xlab('Longitude') +
  ylab('Latitude') +
  geom_label_repel(aes(Long, Lat, label = Kroymann, 
                       fill=Color),
                   data=colors_clades_a,
                   size = 4, force=1,
                   box.padding = 0.5, point.padding = 0.3,
                   segment.color = 'black') +
  theme(text = element_text(size=18)) +
  theme(axis.text=element_text(size=8,color='black'),
        axis.title=element_text(size=12,color='black')) +
  theme(plot.title = element_text(size = 26, vjust=1)) +
  theme(plot.title = element_text(size = 18,face="bold")) +
  theme(legend.key = element_rect(fill = "white")) +
  theme(legend.text = element_text(colour="black", size=8))+
  theme(legend.title = element_text(colour="black", size=10, face="bold"))+
  scale_color_manual(values = c("gold", "green", "red",  "violetred1", "gray44", "blue1",  "purple3", "black"))+
  scale_fill_manual(values=c("black", "blue", "green2", "magenta", "red3", "skyblue", "yellow"))


#Other maps options:
print(mymap)+
  geom_point(data=colors_clades_a, aes(x=Long, y=Lat, shape=Clade, color=Clade), alpha = 7/10, size=3) +
  ggtitle('') +
  xlab('Longitude') +
  ylab('Latitude') +
  theme(text = element_text(size=18)) +
  theme(axis.text=element_text(size=14,color='black'),
        axis.title=element_text(size=20,color='black')) +
  theme(plot.title = element_text(size = 26, vjust=1)) +
  theme(plot.title = element_text(size = 18,face="bold")) +
  theme(legend.key = element_rect(fill = "white")) +
  theme(legend.text = element_text(colour="black", size=16))+
  theme(legend.title = element_text(colour="black", size=14, face="bold"))+
  scale_color_manual(values = c("gold", "green", "red",  "violetred1", "gray44", "blue1",  "purple3", "black"))+
  scale_shape_manual(values = c(15,12, 8,17,19,19,13,19))


print(mymap)+
  geom_point(data=colors_clades_a, aes(x=Long, y=Lat, shape=Clade), alpha = 7/10, size=3) +
  ggtitle('') +
  xlab('Longitude') +
  ylab('Latitude') +
  theme(text = element_text(size=18)) +
  theme(axis.text=element_text(size=14,color='black'),
        axis.title=element_text(size=20,color='black')) +
  theme(plot.title = element_text(size = 26, vjust=1)) +
  theme(plot.title = element_text(size = 18,face="bold")) +
  theme(legend.key = element_rect(fill = "white")) +
  theme(legend.text = element_text(colour="black", size=16))+
  theme(legend.title = element_text(colour="black", size=14, face="bold"))+
  scale_shape_manual(values = c(15,12, 8,17,19,19,13,19))

print(mymap)+
  geom_point(data=colors_clades_a, aes(x=Long, y=Lat, color=Clade), alpha = 7/10, size=3) +
  ggtitle('') +
  xlab('Longitude') +
  ylab('Latitude') +
  theme(text = element_text(size=18)) +
  theme(axis.text=element_text(size=14,color='black'),
        axis.title=element_text(size=20,color='black')) +
  theme(plot.title = element_text(size = 26, vjust=1)) +
  theme(plot.title = element_text(size = 18,face="bold")) +
  theme(legend.key = element_rect(fill = "white")) +
  theme(legend.text = element_text(colour="black", size=16))+
  theme(legend.title = element_text(colour="black", size=14, face="bold"))+
  scale_color_manual(values = c("orchid1", "yellow", "orchid4",  "yellow3", "olivedrab3", "violetred1", "goldenrod4",  "gold"))


print(mymap)+
  geom_point(data=colors_clades_a, aes(x=Long, y=Lat, shape=Clade, color=Clade), alpha = 7/10, size=3) +
  ggtitle('') +
  xlab('Longitude') +
  ylab('Latitude') +
  theme(text = element_text(size=18)) +
  theme(axis.text=element_text(size=14,color='black'),
        axis.title=element_text(size=20,color='black')) +
  theme(plot.title = element_text(size = 26, vjust=1)) +
  theme(plot.title = element_text(size = 18,face="bold")) +
  theme(legend.key = element_rect(fill = "white")) +
  theme(legend.text = element_text(colour="black", size=16))+
  theme(legend.title = element_text(colour="black", size=14, face="bold"))+
  scale_color_manual(values = c("gold", "green", "red",  "violetred1", "gray44", "blue1",  "purple3", "black"))+
  scale_shape_manual(values = c(8,8,8,8,8,8,8,8))

#Phylogeny and population:
setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny/MAM3/With all Kroymann")  
Population <- read.csv("Population.csv")



f<- ggtree(root(tree_AOP2, "Lyrata")) %<+% Population + 
  geom_tippoint(aes(color=group), size=2) +
 
  geom_rootedge(rootedge = .05)+
  xlim(0.07,0.11)+
  theme_tree2()+
  scale_color_manual(values = c("black", "orange", "grey", "gold", "brown", "dodgerblue2", "red", "green", "purple", "pink"))

f





pr.species<-c("9816","9754")
pr.tree<-drop.tip(tree_MAM3_a,
                  setdiff(tree_MAM3_a$tip.label,pr.species))
plotTree(pr.tree,ftype="i")

anolis.noPR<-drop.tip(tree_MAM3_a,pr.species)
tree_MAM3_as<-c(tree_MAM3_a,anolis.noPR,pr.clade,pr.tree)
print(tree_MAM3_as,details=TRUE)

write.tree(tree_MAM3_as,file="example.trees")
cat(readLines("example.trees"),sep="\n")

install.packages("treeio")
library(treeio)

bi_subset <- tree_subset(tree_MAM3_a, "9754", levels_back = 20)
bi_subset
plot(bi_subset) 

plotTree(b, setEnv=TRUE)
b
nodelabels(b)

plotTree(tree_MAM3_a, setEnv = TRUE)
nodelabels()

tt62 <- extract.clade(tree_MAM3_a, 1100)
plotTree(tt62)


Trees<-c(tree_MAM3_a, tree_MAM3_a.noPR,pr.clade,pr.tree_MAM3_a)
print(anolis.trees,details=TRUE)

#Spreading the tree:
node<-fastMRCA(tree_MAM3_a,"7000",
               "6897")
pr.clade<-extract.clade(tree_MAM3_a,node)
plotTree(pr.clade,ftype="i")
plot(tree_MAM3_a)
phylo_clade=as.phylo(pr.clade)

coords_5_1<-coords_3_1[c(-52,-53),]
coords_5 <- coords_5_1[,-1]
rownames(coords_5) <- coords_5_1[,1]
coords_5 = as.matrix(coords_5)


colors_d_3<-colors_d[c(-52,-53,-668,-669,-776),]

colors_d_4 <- as.character(colors_d_3$Color)
names(colors_d_4) <- colors_d_3$Accession
view(colors_d_4)

map_clade = phylo.to.map(phylo_clade,coords_5, colors=colors_d_4, database='world', plot=F, xlim = c(-20, 59),ylim=c(35, 71))



my_phylo2map(map_clade,colors = colors_d_4,
             split = c(.6,.55), # relative vertical sizes of tree and map
             ftype='off',# turn off tree labels
             lwd=.3,lty=1, # size and style of lines in both map and connections
             cex.points=c(.8,1), # size of leaf circles and map circles
             mar=c(0,3,0,0), # margin sizes (see par())
             main = 'MAM3',
             direction = 'downwards' # alternative "rightwards" for tree to the left
) 



pr.clade.1<-extract.clade(tree_MAM3_a, ylim=c(0,20))

d<- ggtree(tree_MAM3_a) %<+% info + 
  geom_tippoint(aes(color=Chemotype), size=2) +
  scale_color_manual(values=cols)+ geom_rootedge(rootedge = .005)+
  theme_tree2() +
  ylim(0,200)
xlim(0.05,0.07) 

d 

plot(tree_MAM3_a)
nodelabels()
tiplabels()


clade <- tree_subset(tree_MAM3_a, node=807, levels_back=0)
clade2 <- tree_subset(tree_MAM3_a, node=962, levels_back=0)
p1 <- ggtree(clade) + geom_tiplab() 
plot(p1)
p2 <- ggtree(clade2) + geom_tiplab() 
plot(p2)



gheatmap(d, info_3, width=0.2, font.size=3, color=NULL, colnames_angle=-45, hjust=0)+
  scale_fill_manual(name='MAM3',values=c(South="deeppink2", North="chartreuse4", "OH-But"="red3", "3OHP"="green2", Allyl="blue", "3MSO"="yellow", "4MSO"="magenta", "4OHB"="skyblue", Butenyl="black",
                                         Lyrata="black",  ARM="maroon", AUT="green1", AZE="green2", BEL="green3", BUL="maroon1", CRO="maroon2", CZE="green4", DEN="green1", ESP="maroon3",
                                         EST="green1", FIN="green2", FRA="green3", GEO="maroon1", GER="green4", GRC="maroon1", IRL="green2", IRN="green2", ITA="maroon", LBN="maroon2", 
                                         LTU="green1", NED="green4", NOR="green4", POL="green1", POR="maroon1", ROU="green2", RUS="green1", SRB="maroon", SUI="green3", SVK="maroon2", SWE="green1", UK="green3",
                                         UKR="green4", UNK="green3", C3="blue", C4="red"))
phylo_d<-as.phylo(d)
plotTree(phylo_d)
nodelabels()
tt62<-extract.clade(phylo_d,792)
plotTree(tt62)

plotTree(phylo_d, ylim=c(0,104))

nodelabels(bg="white")
write.tree(tree_MAM3_a)
species<-c("9978","9982","10004","9987","Lyrata")
pruned.tree<-drop.tip(tree_MAM3_a,tree_MAM3_a$tip.label[-match(species, tree_MAM3_a$tip.label)])
plot(pruned.tree)
write.tree(pruned.tree)


c<- ggtree(root(tree_MAM3, "Lyrata")) %<+% info + 
  geom_tippoint(aes(color=Chemotype), size=2) +
  scale_color_manual(values=cols)+ geom_rootedge(rootedge = .05)+
  geom_tiplab(aes(label=Name), size=3)+
  theme_tree2()+ylim(0,70)
c

write.tree(tree,"example.tre")
cat(readLines("example.tre"))


install.packages("treeio")
library(treeio)
install.packages("ggrtee")
library(ggtree)
bi_subset <- tree_subset(c, "9826", levels_back = 4)

bi_subset %>% 
  ggtree(aes(color = group)) + 
  geom_tiplab() + 
  theme_tree2() + 
  scale_color_manual(values = c(`1` = "red", `0` = "black")) +
  xlim(0, 4)

#Spreading the tree:
node<-fastMRCA(tree_MAM3_a,"9776",
               "9970")
pr.clade<-extract.clade(tree_MAM3_a,node)
plotTree(pr.clade,ftype="i")

phylo_clade=as.phylo(pr.clade)
map_clade = phylo.to.map(phylo_clade,coords_4, database='world', plot=T, xlim = c(-20, 59),ylim=c(35, 71))

#By areas:

c<- ggtree(tree_MAM3_a) +ylim(0,20)
c 

phylo_a = as.phylo(c)
plot(phylo_a)
map_a = phylo.to.map(phylo_a,coords_4,  database='world', plot=TRUE, xlim = c(-20, 59),ylim=c(35, 71))


#Old options:
tree_s<-pbtree(n=10,scale=1)
plotTree(tree_MAM3_a,node.numbers=TRUE)

tree_MAM3_a$node.label<-paste("n",1:tree_MAM3_a$Nnode,sep="")
plotTree(tree_MAM3_a)
nodelabels(tree_MAM3_a$node.label)


