library(TCCLandim)
require(igraph)
require(ggplot2)
require(ggmap)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization


#dados <- read.table("C:/Users/Lander/Desktop/", head = TRUE)
dados <- read.table("H:/O meu disco/TCC_Lander/Bianca/polbooks.txt", head = TRUE)
k=2
s=0.2
R=1

#op <- par(mfrow = c(2, 2),las = 2)


TCCLandim(dados,3,0.04,5)

#mat = matrix(nrow =34,ncol = 100 )
#a = TCCLandim(dados,k,4,R)


  # for (i in c(1:10)) {
  # a <- TCCLandim(dados,k,i/1000,R)$cluster

   #mat[i,]<-a


  # }



#for (i in c(2:5)) {
 # a <- TCCLandim(dados,i,0.04,R)$cluster

  #mat[i,]<-a


#}



















