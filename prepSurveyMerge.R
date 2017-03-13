library(VIM);library(data.table);library(simputation)
l <- fread("eu_lfs_puf/SI_LFS_2013_Q1.csv")
s <- fread("eu_silc_puf/DE_puf_p_silc2013.csv")
p <- fread("piaac/prgdeup1.csv")
