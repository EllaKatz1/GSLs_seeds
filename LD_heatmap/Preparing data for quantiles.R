BiocManager::install("Biostrings")
require(Biostrings)
library(rhdf5)

install.packages("hdf5r")
library(hdf5r)

h5ls("D:/1001_SNP_MATRIX/imputed_snps_binary.hdf5")

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.11")

library(rhdf5)

h5ls("D:/1001_SNP_MATRIX/imputed_snps_binary.h5")
h5ls("D:/1001_SNP_MATRIX/imputed_snps_binary.hdf5")

mydata_1 <- h5ls("D:/1001_SNP_MATRIX/imputed_snps_binary.hdf5")
mydata <- h5read("D:/1001_SNP_MATRIX/imputed_snps_binary.hdf5", "/snps")

  
str(mydata)










setwd("D:/")
details <- read.table(file="1001genomes_snp-short-indel_only_ACGTN.txt", header=T, sep="\t")



fn_1 <- "https://1001genomes.org/data/GMI-MPI/releases/v3.1/SNP_matrix_imputed_hdf5/1001_SNP_MATRIX.tar.gz"
download.file(fn_1,destfile="tmp_1.tar.gz")
untar("tmp_1.tar.gz",list=TRUE)  ## check contents
untar("tmp_1.tar.gz")
## or, if you just want to extract the target file:
untar("tmp_1.tar.gz",files="1001_SNP_MATRIX/imputed_snps_binary.hdf5")
X_1 <- read.table("1001_SNP_MATRIX/imputed_snps_binary.hdf5")






mydata <- h5read("D:/1001_SNP_MATRIX/imputed_snps_binary.hdf5", "/mygroup/mydata")

setwd("D:/1001_SNP_MATRIX/")
file <- h5file("imputed_snps_binary.h5")
fid <- H5Fopen("imputed_snps_binary.h5")


fn_1 <- "https://1001genomes.org/data/GMI-MPI/releases/v3.1//1001genomes_snp-short-indel_with_tair10_only_ACGTN.vcf.gz"
download.file(fn_1,destfile="tmp_1.tar.gz")
untar("tmp_1.tar.gz",list=TRUE)  ## check contents
untar("tmp_1.tar.gz")
## or, if you just want to extract the target file:
untar("tmp_1.tar.gz",files="1001_SNP_MATRIX/md5sum.txt~")
X_1 <- read.table("1001_SNP_MATRIX/md5sum.txt~")


https://1001genomes.org/data/GMI-MPI/releases/v3.1/
