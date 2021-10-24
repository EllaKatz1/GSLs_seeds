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

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("ggtree")


basedir<-("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny/MAM3/With all Kroymann/Bootstraps")

dna_MAM3_boot_1 <- readDNAStringSet(file=file.path(basedir, "Bootstrap_1.txt"), format = "fasta")
dna_MAM3_boot_2 <- readDNAStringSet(file=file.path(basedir, "Bootstrap_2.txt"), format = "fasta")
dna_MAM3_boot_2
alignment_MAM3_boot_2 <- msa(dna_MAM3_boot_2)

dna_MAM3_boot_3 <- readDNAStringSet(file=file.path(basedir, "Bootstrap_3.txt"), format = "fasta")
dna_MAM3_boot_3
alignment_MAM3_boot_3 <- msa(dna_MAM3_boot_3)

dna_MAM3_boot_4 <- readDNAStringSet(file=file.path(basedir, "Bootstrap_4.txt"), format = "fasta")
dna_MAM3_boot_4
alignment_MAM3_boot_4 <- msa(dna_MAM3_boot_4)

dna_MAM3_boot_5 <- readDNAStringSet(file=file.path(basedir, "Bootstrap_5.txt"), format = "fasta")
dna_MAM3_boot_5
alignment_MAM3_boot_5 <- msa(dna_MAM3_boot_5)

dna_MAM3_boot_6 <- readDNAStringSet(file=file.path(basedir, "Bootstrap_6.txt"), format = "fasta")
dna_MAM3_boot_6
alignment_MAM3_boot_6 <- msa(dna_MAM3_boot_6)

dna_MAM3_boot_7 <- readDNAStringSet(file=file.path(basedir, "Bootstrap_7.txt"), format = "fasta")
dna_MAM3_boot_7
alignment_MAM3_boot_7 <- msa(dna_MAM3_boot_7)

dna_MAM3_boot_8 <- readDNAStringSet(file=file.path(basedir, "Bootstrap_8.txt"), format = "fasta")
dna_MAM3_boot_8
alignment_MAM3_boot_8 <- msa(dna_MAM3_boot_8)

#Save the alignment:
library(seqinr)
setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny/MAM3/With all Kroymann/Bootstraps")

alignment2Fasta <- function(alignment, alignment_MAM3_boot_2) {
  sink(alignment_MAM3_boot_2)
  
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
alignment2Fasta(alignment_MAM3_boot_2, 'boot_2.fasta')

phang_msa_MAM3_boot_8 <- as.DNAbin(alignment_MAM3_boot_8, type = "NJ")
phang_msa_MAM3_boot_2 <- as.DNAbin(alignment_MAM3_boot_2, type = "NJ")

#Boot_2:

dm_MAM3_boot_2 <- dist.ml(phang_msa_MAM3_boot_2)        
tree_MAM3_boot_2 <- NJ(dm_MAM3_boot_2)
tree_MAM3_boot_2a<-root(tree_MAM3_boot_2, "Lyrata")

#Save the tree:
install.packages("Rcpp")
install.packages("castor")
Newick_string = write_tree(tree_MAM3_boot_2a)

write.tree(tree_MAM3_boot_1a, file = "Boot_2_tree", append = FALSE,
           digits = 10, tree.names = FALSE)




#tree_MAM3_boot_1b = drop.tip(tree_MAM3_boot_1a,'Lyrata')
#tree_MAM3_boot_1b$edge.length = pmax(0,tree_MAM3_boot_1b$edge.length)

#bp_1 <- boot.phylo(tree_MAM3_boot_1, phang_msa_MAM3_boot_1, function(xx) njs(dist.ml(xx)))
#plot(tree_MAM3_boot_1)
#nodelabels(bp_1)

#phang_msa_bootstrap_MAM3 <- as.DNAbin(alignment_MAM3_boot_1, type = "nj")
#decide if nj or UPGMA:
MAM3_L.tr <- as.DNAbin(alignment_MAM3_boot_2, type = "nj") %>% 
  dist.ml() %>% NJ() %>% root("Lyrata")   

#write.nexus(MAM3_L.tr, file = "TREE.nex")

f <- function(x) njs(dist.ml(x))

# Bootstrap calculation
bp_a<-boot.phylo(phy = MAM3_L.tr, x = phang_msa_MAM3_boot_2, FUN = f, B = 100)
bp_aa<-bp_a
bp_ab<-replace(bp_aa, bp_aa<=60, NA)

bp_c_2<-boot.phylo(phy = MAM3_L.tr, x = phang_msa_MAM3_boot_2, FUN = f, B = 100, trees=TRUE)$trees
## get proportions of each clade:
clad_2 <- prop.clades(MAM3_L.tr, bp_c_2, rooted = TRUE)
clad_a_2<-replace(clad_2, clad_2<=60, NA)


setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny/MAM3/With all Kroymann/Bootstraps")  
Clades_info <- read.csv("Clades_info.csv")
colnames(Clades_info_ab)

setwd("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny/MAM3/With all Kroymann")  
colors_clades_with_2ab <- read.csv("colors_clades_with_2ab.csv")

Clades_info_ab<-merge(Clades_info, colors_clades_with_2ab, by="Accession")

MAM3_boot <- ggtree(tree_MAM3_boot_2a)%<+% Clades_info_ab + 
  geom_tippoint(aes(color=Clade), size=2)+
  #geom_tiplab(linetype=NA,aes(label=Name), size=4)+ 
  #geom_tiplab(linetype=NA, aes(label=Kroymann), size=3, hjust=1.3)+
 scale_color_manual(values = c(Clade_1="#800000",Clade_2a="yellow3", Clade_2b="#808000", Clade_3="#f58231", Clade_4="#3cb44b", Clade_5="#f032e6", Clade_6="#000000", Clade_7="#4363d8", Clade_8="#911eb4"))+
theme_tree2() + geom_rootedge()+xlim(0.035,0.055)+#ylim(40,180)+
   #geom_nodelab(label = bp_ab, geom = 'label', hjust=5)
    geom_nodelab(label =clad_a_2, geom = 'label',hjust=5)

MAM3_boot  

ggsave("C:/Users/Ella Katz/Desktop/01 - Post Doc/Projects/Sarah Turner/Analyses/PCA/Phylogeny/MAM3/With all Kroymann/Bootstraps/Mini_271.jpeg", dpi = 600)

#https://rdrr.io/cran/ape/man/boot.phylo.html

bp_c<-boot.phylo(phy = MAM3_L.tr, x = phang_msa_MAM3_boot_2, FUN = f, B = 100, trees=TRUE)$trees
## get proportions of each clade:
clad <- prop.clades(MAM3_L.tr, bp_c, rooted = TRUE)
clad_a<-replace(clad, clad<=50, NA)
## get proportions of each bipartition:
boot <- prop.clades(MAM3_L.tr, bp_c)
boot_a<-replace(boot, boot<=60, NA)
layout(1)
par(mar = rep(2, 4))
plot(MAM3_L.tr, main = "Bipartition vs. Clade Support Values")
drawSupportOnEdges(boot)
nodelabels(clad_a)
legend("bottomleft", legend = c("Bipartitions", "Clades"), pch = 22,
       pt.bg = c("green", "lightblue"), pt.cex = 2.5)


#Boot_3:

dm_MAM3_boot_3 <- dist.ml(phang_msa_MAM3_boot_3)        
tree_MAM3_boot_3 <- NJ(dm_MAM3_boot_3)
tree_MAM3_boot_3a<-root(tree_MAM3_boot_3, "Lyrata")

MAM3_L.tr_3 <- as.DNAbin(alignment_MAM3_boot_3, type = "nj") %>% 
  dist.ml() %>% NJ() %>% root("Lyrata")   

f <- function(x) njs(dist.ml(x))

# Bootstrap calculation
bp_c_3<-boot.phylo(phy = MAM3_L.tr_3, x = phang_msa_MAM3_boot_3, FUN = f, B = 100, trees=TRUE)$trees
## get proportions of each clade:
clad_3 <- prop.clades(MAM3_L.tr_3, bp_c_3, rooted = TRUE)
clad_a_3<-replace(clad_3, clad_3<=50, NA)



Clades_info <- read.csv("Clades_info.csv")
colnames(Clades_info)

MAM3_boot_3 <- ggtree(tree_MAM3_boot_3a)%<+% Clades_info_ab + 
  geom_tippoint(aes(color=Clade), size=2)+
  #geom_tiplab(linetype=NA,aes(label=Name), size=4)+ 
  #geom_tiplab(linetype=NA, aes(label=Kroymann), size=3, hjust=1.3)+
  scale_color_manual(values = c(Clade_1="#800000", Clade_2="#808000", Clade_3="#f58231", Clade_4="#3cb44b", Clade_5="#f032e6", Clade_6="#000000", Clade_7="#4363d8", Clade_8="#911eb4"))+
    theme_tree2() + geom_rootedge()#+xlim(0.02,0.04)+
  # geom_nodelab(label =bp_c_3, geom = 'label',hjust=9)
  geom_nodelab(label =clad_a_3, geom = 'label',hjust=4)

MAM3_boot_3  


bp_c<-boot.phylo(phy = MAM3_L.tr, x = phang_msa_MAM3_boot_2, FUN = f, B = 100, trees=TRUE)$trees
## get proportions of each clade:
clad <- prop.clades(MAM3_L.tr, bp_c, rooted = TRUE)
clad_a<-replace(clad, clad<=50, NA)



#Boot_4:

dm_MAM3_boot_4 <- dist.ml(phang_msa_MAM3_boot_4)        
tree_MAM3_boot_4 <- NJ(dm_MAM3_boot_4)
tree_MAM3_boot_4a<-root(tree_MAM3_boot_4, "Lyrata")

MAM3_L.tr_4 <- as.DNAbin(alignment_MAM3_boot_4, type = "nj") %>% 
  dist.ml() %>% NJ() %>% root("Lyrata")   

f <- function(x) njs(dist.ml(x))

# Bootstrap calculation
bp_c_4<-boot.phylo(phy = MAM3_L.tr_4, x = phang_msa_MAM3_boot_4, FUN = f, B = 100, trees=TRUE)$trees
## get proportions of each clade:
clad_4 <- prop.clades(MAM3_L.tr_4, bp_c_4, rooted = TRUE)
clad_a_4<-replace(clad_4, clad_4<=60, NA)

#bp_a<-boot.phylo(phy = MAM3_L.tr, x = phang_msa_MAM3_boot_2, FUN = f, B = 100)
#bp_aa<-bp_a
#bp_ab<-replace(bp_aa, bp_aa<=60, NA)

MAM3_boot_4 <- ggtree(tree_MAM3_boot_4a)%<+% Clades_info_ab + 
  geom_tippoint(aes(color=Clade), size=2)+
  #geom_tiplab(linetype=NA,aes(label=Name), size=4)+ 
  #geom_tiplab(linetype=NA, aes(label=Kroymann), size=3, hjust=1.3)+
  scale_color_manual(values = c(Clade_1="#800000", Clade_2a="yellow3", Clade_2b="#808000", Clade_3="#f58231", Clade_4="#3cb44b", Clade_5="#f032e6", Clade_6="#000000", Clade_7="#4363d8", Clade_8="#911eb4"))+
  theme_tree2() + geom_rootedge()+xlim(0.055,0.075)+
  # geom_nodelab(label =bp_c_3, geom = 'label',hjust=9)
  geom_nodelab(label =clad_a_4, geom = 'label',hjust=4)

MAM3_boot_4  


bp_c<-boot.phylo(phy = MAM3_L.tr, x = phang_msa_MAM3_boot_2, FUN = f, B = 100, trees=TRUE)$trees
## get proportions of each clade:
clad <- prop.clades(MAM3_L.tr, bp_c, rooted = TRUE)
clad_a<-replace(clad, clad<=50, NA)

#Boot_5:

dm_MAM3_boot_5 <- dist.ml(phang_msa_MAM3_boot_5)        
tree_MAM3_boot_5 <- NJ(dm_MAM3_boot_5)
tree_MAM3_boot_5a<-root(tree_MAM3_boot_5, "Lyrata")

MAM3_L.tr_5 <- as.DNAbin(alignment_MAM3_boot_5, type = "nj") %>% 
  dist.ml() %>% NJ() %>% root("Lyrata")   

f <- function(x) njs(dist.ml(x))

# Bootstrap calculation
bp_c_5<-boot.phylo(phy = MAM3_L.tr_5, x = phang_msa_MAM3_boot_5, FUN = f, B = 100, trees=TRUE)$trees
## get proportions of each clade:
clad_5 <- prop.clades(MAM3_L.tr_5, bp_c_5, rooted = TRUE)
clad_a_5<-replace(clad_5_1, clad_5<=50, NA)


MAM3_boot_5 <- ggtree(tree_MAM3_boot_5a)%<+% Clades_info + 
  geom_tippoint(aes(color=Clade.number), size=2)+
  #geom_tiplab(linetype=NA,aes(label=Name), size=4)+ 
  geom_tiplab(linetype=NA, aes(label=Kroymann), size=3, hjust=1.3)+
  scale_color_manual(values = c(Clade_1="#800000", Clade_2="#808000", Clade_3="#f58231", Clade_4="#3cb44b", Clade_5="#f032e6", Clade_6="#000000", Clade_7="#4363d8", Clade_8="#911eb4"))+
  theme_tree2() + geom_rootedge()+xlim(0.02,0.035)+
  # geom_nodelab(label =bp_c_3, geom = 'label',hjust=9)
  geom_nodelab(label =clad_a_5_1, geom = 'label',hjust=4)

MAM3_boot_5  


bp_c<-boot.phylo(phy = MAM3_L.tr, x = phang_msa_MAM3_boot_2, FUN = f, B = 100, trees=TRUE)$trees
## get proportions of each clade:
clad <- prop.clades(MAM3_L.tr, bp_c, rooted = TRUE)
clad_a<-replace(clad, clad<=50, NA)

#Boot_6:

dm_MAM3_boot_6 <- dist.ml(phang_msa_MAM3_boot_6)        
tree_MAM3_boot_6 <- NJ(dm_MAM3_boot_6)
tree_MAM3_boot_6a<-root(tree_MAM3_boot_6, "Lyrata")

MAM3_L.tr_6 <- as.DNAbin(alignment_MAM3_boot_6, type = "nj") %>% 
  dist.ml() %>% NJ() %>% root("Lyrata")   

f <- function(x) njs(dist.ml(x))

# Bootstrap calculation
bp_c_6<-boot.phylo(phy = MAM3_L.tr_6, x = phang_msa_MAM3_boot_6, FUN = f, B = 100, trees=TRUE)$trees
## get proportions of each clade:
clad_6 <- prop.clades(MAM3_L.tr_6, bp_c_6, rooted = TRUE)
clad_a_6<-replace(clad_6, clad_6<=50, NA)


MAM3_boot_6 <- ggtree(tree_MAM3_boot_6a)%<+% Clades_info + 
  geom_tippoint(aes(color=Clade.number), size=2)+
  geom_tiplab(linetype=NA,aes(label=Name), size=4)+ 
  geom_tiplab(linetype=NA, aes(label=Kroymann), size=3, hjust=1.3)+
  scale_color_manual(values = c(Clade_1="#800000", Clade_2="#808000", Clade_3="#f58231", Clade_4="#3cb44b", Clade_5="#f032e6", Clade_6="#000000", Clade_7="#4363d8", Clade_8="#911eb4"))+
  theme_tree2() + geom_rootedge()+xlim(0.035,0.05)+
  # geom_nodelab(label =bp_c_3, geom = 'label',hjust=9)
  geom_nodelab(label =clad_a_6, geom = 'label',hjust=8)

MAM3_boot_6  


#Boot_7:

dm_MAM3_boot_7 <- dist.ml(phang_msa_MAM3_boot_7)        
tree_MAM3_boot_7 <- NJ(dm_MAM3_boot_7)
tree_MAM3_boot_7a<-root(tree_MAM3_boot_7, "Lyrata")

MAM3_L.tr_7 <- as.DNAbin(alignment_MAM3_boot_7, type = "nj") %>% 
  dist.ml() %>% NJ() %>% root("Lyrata")   

f <- function(x) njs(dist.ml(x))

# Bootstrap calculation
bp_c_7<-boot.phylo(phy = MAM3_L.tr_7, x = phang_msa_MAM3_boot_7, FUN = f, B = 100, trees=TRUE)$trees
## get proportions of each clade:
clad_7 <- prop.clades(MAM3_L.tr_7, bp_c_7, rooted = TRUE)
clad_a_7<-replace(clad_7, clad_7<=50, NA)


MAM3_boot_7 <- ggtree(tree_MAM3_boot_7a)%<+% Clades_info + 
  geom_tippoint(aes(color=Clade.number), size=2)+
  #geom_tiplab(linetype=NA,aes(label=Name), size=4)+ 
  geom_tiplab(linetype=NA, aes(label=Kroymann), size=3, hjust=1.3)+
  scale_color_manual(values = c(Clade_1="#800000", Clade_2="#808000", Clade_3="#f58231", Clade_4="#3cb44b", Clade_5="#f032e6", Clade_6="#000000", Clade_7="#4363d8", Clade_8="#911eb4"))+
  theme_tree2() + geom_rootedge()+xlim(0.035,0.046)+#ylim(0,310)+
  # geom_nodelab(label =bp_c_3, geom = 'label',hjust=9)
  geom_nodelab(label =clad_a_7, geom = 'label',hjust=9)

MAM3_boot_7  


Clades_info_1<-Clades_info[,c(1,3,8)]
row.names(Clades_info_1) <- Clades_info_1$Accession
Clades_info_2<-Clades_info_1[,c(2:3)]

gheatmap(MAM3_boot_7, Clades_info_2, width=0.2, font.size=3, color=NULL, colnames_angle=-45, hjust=0)
  scale_fill_manual(values=c(South="deeppink2", North="chartreuse4", "OH-But"="red3", "3OHP"="green2", Allyl="blue", "3MSO"="yellow", "4MSO"="magenta", "4OHB"="skyblue", Butenyl="black",
                             Lyrata="white", "Col-0"="white", "Sorbo"="white", "Cvi-0"="white", "Ler-0"="white", "NA"="white", C3="blue", C4="red"))

#Boot_8:
  
dm_MAM3_boot_8 <- dist.ml(phang_msa_MAM3_boot_8)        
tree_MAM3_boot_8 <- NJ(dm_MAM3_boot_8)
tree_MAM3_boot_8a<-root(tree_MAM3_boot_8, "Lyrata")
  
MAM3_L.tr_8 <- as.DNAbin(alignment_MAM3_boot_8, type = "nj") %>% 
  dist.ml() %>% NJ() %>% root("Lyrata")   
  
f <- function(x) njs(dist.ml(x))
  
# Bootstrap calculation
bp_c_8<-boot.phylo(phy = MAM3_L.tr_8, x = phang_msa_MAM3_boot_8, FUN = f, B = 1000, trees=TRUE)$trees
## get proportions of each clade:
clad_8 <- prop.clades(MAM3_L.tr_8, bp_c_8, rooted = TRUE)
clad_a_8<-replace(clad_8, clad_8<=300, NA)

bp_c_8a<-boot.phylo(phy = MAM3_L.tr_8, x = phang_msa_MAM3_boot_8, FUN = f, B = 100, trees=TRUE)$trees
## get proportions of each clade:
clad_8a <- prop.clades(MAM3_L.tr_8, bp_c_8a, rooted = TRUE)
clad_a_8a<-replace(clad_8a, clad_8a<=40, NA)
  
  
MAM3_boot_8 <- ggtree(tree_MAM3_boot_8a)%<+% Clades_info + 
    geom_tippoint(aes(color=Clade.number), size=2)+
    #geom_tiplab(linetype=NA,aes(label=Name), size=4)+ 
    geom_tiplab(linetype=NA, aes(label=Kroymann), size=4, hjust=2)+
    scale_color_manual(values = c(Clade_1="#800000", Clade_2="#808000", Clade_3="#f58231", Clade_4="#3cb44b", Clade_5="#f032e6", Clade_6="#000000", Clade_7="#4363d8", Clade_8="#911eb4"))+
    theme_tree2() + geom_rootedge()+xlim(0.034,0.049)#+ylim(0,320)+
    # geom_nodelab(label =bp_c_3, geom = 'label',hjust=9)
    geom_nodelab(label =clad_8, geom = 'label',hjust=8)
  
MAM3_boot_8  
  

xlim(0.034,0.049)
  
Clades_info_1<-Clades_info[,c(1,3,8)]
row.names(Clades_info_1) <- Clades_info_1$Accession
Clades_info_2<-Clades_info_1[,c(2:3)]
  
gheatmap(MAM3_boot_7, Clades_info_2, width=0.2, font.size=3, color=NULL, colnames_angle=-45, hjust=0)
scale_fill_manual(values=c(South="deeppink2", North="chartreuse4", "OH-But"="red3", "3OHP"="green2", Allyl="blue", "3MSO"="yellow", "4MSO"="magenta", "4OHB"="skyblue", Butenyl="black",
                             Lyrata="white", "Col-0"="white", "Sorbo"="white", "Cvi-0"="white", "Ler-0"="white", "NA"="white", C3="blue", C4="red"))
  
  
  

install.packages("ggmsa")
library("ggmsa")
ggmsa(dna_MAM3_boot_1, 10,20, color = "Clustal")

data = tidy_msa(dna_MAM3_boot_1, 0, 20)
MAM3_boot + geom_facet(geom = geom_msa, data = data,  panel = 'msa',
                         color = "Clustal") 





dist=vegdist(gel, method="jaccard", binary=TRUE, diag=FALSE, upper=FALSE, na.rm = FALSE)
dendro=hclust(dist,"complete")
dendro2<-as.phylo(dendro,cex=0.5)
boot.phylo(dendro2,gel,make.tree,B=100,rooted=TRUE)
