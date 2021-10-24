BiocManager::install("Biostrings")
require(Biostrings)
install.packages("tidyverse")
library(tidyverse)
library(msa)
library(phangorn)
library(Biostrings)
library(ggtree)        
library(ggplot2)
install.packages("ggimage")
#install.packages("ape")
library(ape)
install.packages("geiger")
#install.packages("ape", dependencies = TRUE, INSTALL_opts = '--no-lock')
install.packages("geiger", dependencies = TRUE, INSTALL_opts = '--no-lock')
library(ggimage)
install.packages("ggrepel")
library(ggrepel)
install.packages("CRAN")
#install.packages("dplyr")
library(dplyr)
install.packages("gheatmap")

library(tibble)

basedir<-("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny/MAM3/With all Kroymann")


dna_MAM3_L <- readDNAStringSet(file=file.path(basedir, "MAM3_Collection_Lyrata_Kroymann.txt"), format = "fasta")
dna_MAM3_L
alignment_MAM3_L <- msa(dna_MAM3_L)

#Saving the alignment:
library(seqinr)

alignment2Fasta <- function(alignment, alignment_MAM3_L) {
  sink(alignment_MAM3_L)
  
  n <- length(rownames(alignment))
  for(i in seq(1, n)) {
    cat(paste0('>', rownames(alignment)[i]))
    cat('\n')
    the.sequence <- toString(unmasked(alignment)[[i]])
    cat(the.sequence)
    cat('\n')  
  }
  
  sink(NULL)
}
alignment2Fasta(alignment_MAM3_L, 'MAM3_alignment.fasta')

phang_msa_MAM3 <- as.phyDat(alignment_MAM3_L, type = "dna")

dm_MAM3 <- dist.ml(phang_msa_MAM3)        

tree_MAM3 <- NJ(dm_MAM3)

ggtree(tree_MAM3)+ geom_tiplab()


phang_msa_MAM_1 <- as.DNAbin(alignment_MAM3_L, type = "nj")
dm_MAM_L <- dist.ml(phang_msa_MAM_1)        
tree_MAM_L <- NJ(dm_MAM_L)

#Bootstrap:

MAM3_L.tr <- as.DNAbin(alignment_MAM3_L, type = "UPGMA") %>% 
  dist.ml() %>% NJ() %>% root("Lyrata")   

phang_msa_MAM3_1 <- as.DNAbin(alignment_MAM3_L, type = "UPGMA")

f <- function(x) njs(dist.ml(x))

# Bootstrap calculation
bp_a<-boot.phylo(phy = MAM3_L.tr, x = phang_msa_MAM3_1, FUN = f, B = 100)
view(bp_a)
bp_aa<-bp_a
bp_ab<-replace(bp_aa, bp_aa<=60, NA)

#Bootstrap with nodes:
bp_c<-boot.phylo(phy = MAM3_L.tr, x = phang_msa_MAM3_1, FUN = f, B = 1000, trees=TRUE)$trees
## get proportions of each clade:
clad <- prop.clades(MAM3_L.tr, bp_c, rooted = TRUE)
clad_a<-replace(clad, clad<=50, NA)
## get proportions of each bipartition:
boot <- prop.clades(MAM3_L.tr, bp_c)
boot_a<-replace(boot, boot<=60, NA)



setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny/MAM3/With all Kroymann")  
info <- read.csv("Coords_all_K.csv")
cols <- c(South="deeppink2", North="chartreuse4", "OH-But"="red3", "3OHP"="green2", Allyl="blue", "3MSO"="yellow", "4MSO"="magenta", "4OHB"="skyblue", Butenyl="black", Lyrata="black",
          Sorbo="white", "Col-0"="white", "Cvi-0"="white", Unknown="black", "Ler-0"="white")

#Best two:
tree_MAM3_b<-root(tree_MAM3, "Lyrata")
plot(tree_MAM3_b)

tree_MAM3_b <- reorder(tree_MAM3_b)

MAM3 <- ggtree(tree_MAM3_b) %<+% info + 
  geom_tippoint(aes(color=Chemotype), size=2)+
  #geom_tiplab(linetype=NA,aes(color=Chemotype, label=Name), size=4) +
  geom_tiplab(linetype=NA, aes(label=Kroymann), size=3, hjust=1.3)+
  scale_color_manual(values=cols)+
  theme_tree2() + geom_rootedge() +xlim(0.05,0.065)+
#geom_nodelab(label = bp_ab, geom = 'label', hjust=1.5)
geom_nodelab(label = clad_a, geom = 'label', hjust=6.5)

MAM3

plot(tree_MAM3_b, label.offset=0.1)
nodelabels()

e<- ggtree(root(tree_MAM3, "Lyrata")) %<+% info + 
  geom_tippoint(aes(color=Chemotype), size=2) +
  scale_color_manual(values=cols)+ geom_rootedge(rootedge = .05)+
  geom_strip('6434', '9816', barsize=2, color='gold', 
             label="Clade 1", offset=-0.186, fontsize=8) + 
  geom_strip('5486', '9883', barsize=2, color='green', 
             label="Clade 2", offset=-0.187, fontsize=8) + 
  geom_strip('7223', '9725', barsize=2, color='red', 
             label="Clade 3", offset=-0.188, fontsize=8) + 
  geom_strip('6968', '9593', barsize=2, color='red', 
             label="Clade 4", offset=-0.1883, fontsize=6) + 
  geom_strip('9554', '6911', barsize=2, color='red', 
             label="Clade 5", offset=-0.1886, fontsize=6) + 
    geom_strip('9902', '7164', barsize=2, color='navy', 
             label="Clade 6", offset=-0.189, fontsize=8) + 
    geom_strip('9537', '9655', barsize=2, color='purple3', 
             label="Clade 7", offset=-0.19, fontsize=8) +
  geom_strip('9085', '9910', barsize=2, color='black', 
             label="Clade 8", offset=-0.191, fontsize=8) +
  xlim(0.05,0.065)+
    geom_tiplab(linetype=NA, aes(label=Kroymann), size=4,  hjust=1.5)+
  theme_tree2()

e  




colnames(info)
#Heat maps:
info_1<-info[,c(7,4)]
info_2<-info[,c(1,2,7)]
row.names(info_2) <- info_2$Accession
info_3<-info_2[,c(2:3)]
info_3$C3_C4 <- NA

info_3$C3_C4[which(info_3$Chemotype == "3MSO")] <- "C3"
info_3$C3_C4[which(info_3$Chemotype == "3OHP")] <- "C3"
info_3$C3_C4[which(info_3$Chemotype == "Allyl")] <- "C3"
info_3$C3_C4[which(info_3$Chemotype == "4MSO")] <- "C4"
info_3$C3_C4[which(info_3$Chemotype == "4OHB")] <- "C4"
info_3$C3_C4[which(info_3$Chemotype == "Butenyl")] <- "C4"
info_3$C3_C4[which(info_3$Chemotype == "OH-But")] <- "C4"

info_3<-info_3[,c(1,3,2)]
colnames(info_3)
info_4<-info_3[,c(1,2,3)]

a<- ggtree(tree_MAM3_b) %<+% info + 
  geom_tippoint(aes(color=Chemotype), size=2) +
  scale_color_manual(values=cols)+ geom_rootedge(rootedge = .05)+
  geom_tiplab(linetype=NA, aes(label=Kroymann), size=3, hjust=-1.3)+
  theme_tree2()

a  

gheatmap(a, info_3, width=0.2, font.size=3, color=NULL, colnames_angle=-45, hjust=0)+
  scale_fill_manual(values=c(South="deeppink2", North="chartreuse4", "OH-But"="red3", "3OHP"="green2", Allyl="blue", "3MSO"="yellow", "4MSO"="magenta", "4OHB"="skyblue", Butenyl="black",
                             Lyrata="white", "Col-0"="white", "Sorbo"="white", "Cvi-0"="white", "Ler-0"="white", "NA"="white", C3="blue", C4="red"))

ggsave("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny/MAM3/With all Kroymann/Tree with heat map.jpeg", dpi = 600)

#South:
gheatmap(a, info_3, width=0.2, font.size=3, color=NULL, colnames_angle=-45, hjust=0)+
  scale_fill_manual(name='South', breaks=c("C3", "C4", "N", "S", "ARM", "BUL", "CRO", "ESP", "GEO", "GRC", "ITA", "LBN", "POR", "SRB", "SVK"),  
                    values=c(S="deeppink2", N="chartreuse4", "OH-But"="red3", "3OHP"="green2", Allyl="blue", "3MSO"="yellow", "4MSO"="magenta", "4OHB"="skyblue", Butenyl="black",
                             Lyrata="black",ARM="magenta", AUT="white", AZE="white", BEL="white", BUL="magenta", CRO="magenta", CZE="white", DEN="white", ESP="red4",
                             EST="white", FIN="white", FRA="white", GEO="magenta", GER="white", GRC="magenta", IRL="white", IRN="magenta", ITA="gold", LBN="magenta", 
                             LTU="white", NED="white", NOR="white", POL="white", POR="red4", ROU="white", RUS="white", SRB="magenta", SUI="white", SVK="magenta", SWE="white", UK="white",
                             UKR="white", UNK="white", C3="blue", C4="red"))

#North:
gheatmap(a, info_3, width=0.2, font.size=3, color=NULL, colnames_angle=-45, hjust=0)+
  scale_fill_manual(name='North', breaks=c("C3", "C4", "N", "S", "AUT", "BEL", "CZE", "EST", "FIN", "FRA", "GER", "IRL", "LTU", "NED", "NOR", "POL", "RUS", "SUI", "SWE", "UK", "UKR", "UNK", "DEN"),
                    values=c(S="deeppink2", N="chartreuse4", "OH-But"="red3", "3OHP"="green2", Allyl="blue", "3MSO"="yellow", "4MSO"="magenta", "4OHB"="skyblue", Butenyl="black",
                             Lyrata="black", ARM="white", AUT="red4", AZE="white", BEL="red4", BUL="white", CRO="white", CZE="red4", DEN="gold", ESP="white",
                             EST="red4", FIN="gold", FRA="red4", GEO="white", GER="red4", GRC="white", IRL="magenta", IRN="white", ITA="white", LBN="white", 
                             LTU="red4", NED="red4", NOR="gold", POL="red4", POR="white", ROU="white", RUS="red4", SRB="white", SUI="red4", SVK="white", SWE="gold", UK="magenta",
                             UKR="red4", UNK="red4", C3="blue", C4="red"))

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

tree_MAM3_a<-root(tree_MAM3, "Lyrata")
phylo_3 = as.phylo(tree_MAM3_a)


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
colors_d_1<-colors_d[c(-668,-669,-776, -791, -790, -788,-789, -787),]

colors_d_2 <- as.character(colors_d_1$Color)
names(colors_d_2) <- colors_d_1$Accession
view(colors_d_2)


plot(phylo_3, length = 1)
#colors_b<- c("7000"="magenta", "6897"="red3", "7061"="red3", "7096"="green2", "7109"="red3", "8214"="red3", "7217"="magenta", "Lyrata"="black", "7273"="green2", 
#             "7296"="green2", "8357"="magenta", "7333"="green2", "7411"="blue")

coords_3 <- info[,c(1,5,6)]
#Removing accessions without seq:
coords_3_1<-coords_3[c(-668,-669,-776, -791, -790, -788,-789, -787),]

coords_4 <- coords_3_1[,-1]
rownames(coords_4) <- coords_3_1[,1]
coords_4 = as.matrix(coords_4)

#Droping the Lyrata, Col, Sorbo, Cvi, Ler:
tree_MAM3_b = drop.tip(tree_MAM3_a,'Lyrata')
tree_MAM3_b = drop.tip(tree_MAM3_b,'6909')
tree_MAM3_b = drop.tip(tree_MAM3_b,'6963')
tree_MAM3_b = drop.tip(tree_MAM3_b,'6911')
tree_MAM3_b = drop.tip(tree_MAM3_b,'7203')


phylo_b = as.phylo(tree_MAM3_b)
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
             cex.points=c(1.8,1), # size of leaf circles and map circles
             mar=c(0,3,0,0), # margin sizes (see par())
             main = 'MAM 3',
             rotate=FALSE,
             direction = 'downwards' # alternative "rightwards" for tree to the left
             ) 
plot(tree_MAM3_b, use.edge.length = FALSE)


#Subseting the tree:

#By clades:

MAM3 +xlim(0.025,0.075)
MAM3+ylim(202,399)+ geom_tiplab(linetype=1 ,aes(label=Name), size=3)+xlim(0.05,0.07)
ggtree(tree_MAM3_a) + geom_text(aes(label=node), hjust=-.3)+ ylim(100,200)+xlim(0.05,0.07)


#The clades: 
e<- ggtree(root(tree_MAM3, "Lyrata")) %<+% info + 
  geom_tippoint(aes(color=Chemotype), size=2) +
  scale_color_manual(values=cols)+ geom_rootedge(rootedge = .05)+
  geom_strip('6434', '9816', barsize=2, color="#800000", 
             label="Clade 1", offset=-0.187, fontsize=6, hjust=-0.05) + 
  geom_strip('5486', '9883', barsize=2, color="#808000", 
             label="Clade 2", offset=-0.187, fontsize=6, hjust=-0.05) + 
  geom_strip('7223', '9725', barsize=2, color="#f58231", 
             label="Clade 3", offset=-0.188, fontsize=6, hjust=-0.05) + 
  geom_strip('6968', '9593', barsize=2, color="#3cb44b", 
             label="Clade 4", offset=-0.1883, fontsize=6, hjust=-0.05) + 
  geom_strip('9554', '6911', barsize=2, color="#f032e6", 
             label="Clade 5", offset=-0.1886, fontsize=6, hjust=-0.05) + 
  geom_strip('9902', '7164', barsize=2, color="#000000", 
             label="Clade 6", offset=-0.189, fontsize=6, hjust=-0.05) + 
  geom_strip('9537', '9655', barsize=2, color="#4363d8", 
             label="Clade 7", offset=-0.19, fontsize=6, hjust=-0.05) +
  geom_strip('9085', '9910', barsize=2, color="#911eb4", 
             label="Clade 8", offset=-0.191, fontsize=6, hjust=-0.05) +
  xlim(0.05,0.065)+
  geom_tiplab(linetype=NA, aes(label=Kroymann), size=4,  hjust=1.5)+
  theme_tree2()+
  geom_nodelab(label = clad_a, geom = 'label', hjust=6)

e  

ggsave("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny/MAM3/With all Kroymann/Tree with heat map.jpeg", dpi = 600)


gheatmap(e, info_3, width=0.2, font.size=3, color=NULL, colnames_angle=-45, hjust=0)+
  scale_fill_manual(values=c(South="deeppink2", North="chartreuse4", "OH-But"="red3", "3OHP"="green2", Allyl="blue", "3MSO"="yellow", "4MSO"="magenta", "4OHB"="skyblue", Butenyl="black",
                             Lyrata="white", "Col-0"="white", "Sorbo"="white", "Cvi-0"="white", "Ler-0"="white", "NA"="white", C3="blue", C4="red"))


node_1<-fastMRCA(tree_MAM3_a,'6434', '9816')
pr.clade_1<-extract.clade(tree_MAM3_a,node_1)
plotTree(pr.clade_1,ftype="i")

write.csv(pr.clade_1$tip.label,"C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny/MAM3/With all Kroymann/Clade_1.csv", row.names = FALSE)

setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny/MAM3/With all Kroymann")  
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


e

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

colors_clades <- read.csv("colors_clades.csv")

colors_clades_a<- merge(colors_clades, coords_3, by='Accession')
colnames(info)
Kroymann<-info[,c(1,9)]
colors_clades_a<- merge(colors_clades_a, Kroymann, by='Accession')

Clades_info_a<-Clades_info[,c(1,3)]
colors_clades_a1<- merge(colors_clades_a, Clades_info_a, by='Accession')

colors_clades_b<- merge(colors_clades, info, by='Accession')
write.csv(colors_clades_b,"C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny/MAM3/With all Kroymann/Bootstraps/Clades_info.csv", row.names = FALSE)


#Maps:
install.packages('ggmap')
library(ggmap)
install.packages('CRAN')
library("rjson")
install.packages("maps")
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

#5d637d for a darker ocean

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

#AIzaSyAYVck1dRnEJY0Sfzsb9i5K9gWqlwExITI

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


print(mymap)+
  geom_point(data=colors_clades_a, aes(x=Long, y=Lat, color=Clade.number), alpha = 6/10, size=3) +
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
  scale_color_manual(values = c("#800000", "#808000", "#f58231", "#3cb44b", "#f032e6", "#000000", "#4363d8", "#911eb4"))

ggsave("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/Manuscript/Figures/More plots/Clades_map_b.jpeg", dpi = 600)


#Phylogeny and population:
setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny/MAM3/With all Kroymann")  
Population <- read.csv("Population.csv")



f<- ggtree(root(tree_MAM3, "Lyrata")) %<+% Population + 
  geom_tippoint(aes(color=group), size=2) +
  geom_strip('6434', '9816', barsize=2, color="#800000", 
             label="Clade 1", offset=-0.186, fontsize=8) + 
  geom_strip('5486', '9883', barsize=2, color="#808000", 
             label="Clade 2", offset=-0.187, fontsize=8) + 
  geom_strip('7223', '9725', barsize=2, color="#f58231", 
             label="Clade 3", offset=-0.188, fontsize=8) + 
  geom_strip('6968', '9593', barsize=2, color="#3cb44b", 
             label="Clade 4", offset=-0.1883, fontsize=6) + 
  geom_strip('9554', '6911', barsize=2, color="#f032e6", 
             label="Clade 5", offset=-0.1886, fontsize=6) + 
  geom_strip('9902', '7164', barsize=2, color="#000000", 
             label="Clade 6", offset=-0.189, fontsize=8) + 
  geom_strip('9537', '9655', barsize=2, color="#4363d8", 
             label="Clade 7", offset=-0.19, fontsize=8) +
  geom_strip('9085', '9910', barsize=2, color="#911eb4", 
             label="Clade 8", offset=-0.191, fontsize=8) +
  geom_rootedge(rootedge = .05)+
   xlim(0.05,0.065)+
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


