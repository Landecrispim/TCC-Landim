# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


require(igraph)
require(ggplot2)
require(ggmap)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

dec_espectral <- function(matriz_familia_1){
  n_eig = k
  ei = eigen(matriz_familia_1)
  n = nrow(matriz_familia_1)
  return(ei$vectors[,(n - k + 1):(n)])  ## 12 - 2 >> 12 - 1
}


TCCLandim <- function(dados,K,s,Plot) {
  dados <- dados
  k=K
  s=s
  R=Plot

  A <- matrix(nrow = max(dados),ncol = max(dados))
  D <- matrix(nrow = max(dados),ncol = max(dados))
  for (i in c(1:nrow(dados))) {

    A[dados$A[i],dados$B[i]] <- 1
    A[dados$B[i],dados$A[i]] <- 1

  }



  A[is.na(A)] <- 0


  for (i in c(1:nrow(A))) {
    sum_row = 0
    for (j in c(1:ncol(A))) {
      sum_row <- sum_row + A[i,j]
    }
    D[i,i] = sum_row
    #print(sum_row)
  }

  for (i in c(1:nrow(A))) {
    for (j in c(1:ncol(A))) {
      if(is.na(D[i,j])) D[i,j] <- 0
    }

  }


  #length(eigen(Lf)$values)


  n = nrow(A)        # número de vértices ou número de linhas da matriz de adjacência
  Id = diag(1,n)

  M = solve((1-s)*Id +s*D)
  L = D - A
  Lf = M%*%L





  G=A

  auto_cluster = dec_espectral(Lf)


  #print(auto_cluster)

  auto_cluster = round(auto_cluster, 6)

  km <- kmeans(auto_cluster, k, nstart=50)
  n_clusters <- max(km$cluster)
  n_clusters
  #plot(df$x, df$y, col = km$cluster, pch=20, cex = 1)
  ##fviz_cluster(km, geom = c("point"),ellipse.type = "euclid")
  indices = 48+(k)
  ##plot(df$x,df$y, col=1, pch = km$cluster, cex=1.25)
  #auto_cluster

  km$cluster

  #-----------------------------------------------------------------------

  #km$cluster <- rep(1,times=length(km$cluster))
  #ordenar formação de grupos

  map <- c()


  whileVar <- 1
  while (whileVar<=length(km$cluster)) {
    if(whileVar==1){
      map <- append(map,km$cluster[whileVar])
      #print("entrei eqal")
    }else {
          #print(map,km$cluster)

          if(!(km$cluster[whileVar] %in% map)){
              map <- append(map,km$cluster[whileVar])
              #print("entrei dif")




          }
     # print(km$cluster[whileVar])
     # print(map)
     # print("map")
    }
    whileVar <- whileVar +1

    #print("Flag while")

  }
  whileVar <- 1
  while (whileVar<=length(km$cluster)) {


  km$cluster[whileVar]<-match(km$cluster[whileVar],map)

    whileVar <- whileVar +1
  }



  #-----------------------------------------------------------------------




  ggg <- graph.adjacency(A, mode = "undirected", weighted=TRUE)
  V(ggg)$color <- km$cluster




  if (R==1) {
    plot(ggg,layout = layout_with_kk)
  }
  if (R==2) {
    plot(c(length(eigen(Lf)$values):1),eigen(Lf)$values)

  }
  if (R==3) {
    plot(c(1:length(km$cluster)),km$cluster)


  }
  if (R==4) {
    a = length(km$cluster)
    plot(auto_cluster[1:a,k], type = "l")
    print(a)


  }

  if (R==5) {



   plot(fviz_silhouette(silhouette(km$cluster,dist(auto_cluster))))
    return(fviz_silhouette(silhouette(km$cluster,dist(auto_cluster))))

   # plot(silhouette(km$cluster,dist(auto_cluster)))
  }
  if(R==6){

    ggg <- graph.adjacency(A, mode = "undirected", weighted=TRUE)
    plot(ggg,layout = layout_with_kk)

    print("L")
    print(L)

    print("D")
    print(D)

    print("A")
    print(A)


    print("Lf")
    print(Lf)


    print("M")
    print(M)


    print("M")
    print(Id)


  }



  #return(km)

}
