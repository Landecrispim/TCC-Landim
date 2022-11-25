
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

norma <- function(vetor){
  aux = 0
  for(i in 1:length(vetor)){
    aux = vetor[i]^2 + aux
  }
  norma = sqrt(aux)
  return(norma)
}

similaridade <- function(matriz){
  S <- matrix(NA, nrow = length(matriz[,1]), ncol = length(matriz[,1]))
  for(i in 1:length(matriz[,1])){
    for(j in 1:length(matriz[,1])){
      vetor_aux <- matriz[i,] - matriz[j,]
      S[i,j] <- norma(vetor_aux)
    }
  }
  return(S)
}


gauss_similaridade <- function(matriz_similaridade, sigma, limiar){
  Gauss <- matrix(NA, nrow = length(matriz_similaridade[,1]), ncol = length(matriz_similaridade[,1]))
  for (i in 1:length(matriz_similaridade[,1])){
    for (j in 1:length(matriz_similaridade[,1])){
      Gauss[i,j] <- exp(-(matriz_similaridade[i,j]^2)/(2*sigma^2))
      if (Gauss[i,j] < limiar){
        Gauss[i,j] = 0
      }
    }
  }

  return(Gauss)
}


familia_1 <- function(A)  ## O parâmetro alpha mudou de nome: agora é parâmetro s!!
{
  graus = rowSums(G) # graus dos vértices
  n = nrow(A)        # número de vértices ou número de linhas da matriz de adjacência
  Id = diag(1,n)
  #print(Id)
  D = diag(graus)
  #print(D)
  L = D - G
  M_f1 = solve((1-s)*Id +s*D)
  #print(M_f1)
  L_f1 = M_f1%*%L
  #print(L_f1)
  {
    return(L_f1)
  }
}

dec_espectral <- function(matriz_familia_1){
  n_eig = k
  ei = eigen(matriz_familia_1)
  n = nrow(matriz_familia_1)
  return(ei$vectors[,(n - k + 1):(n)])  ## 12 - 2 >> 12 - 1
}



SpectralGrouping <- function(src_data,head,seed){

  set.seed(seed)
  dados <- read.table(src_data, head = TRUE)
  matriz_dados <- cbind(as.matrix(dados[,1]),as.matrix(dados[,2]))
  df <- data.frame(x = dados$coord_x, y = dados$coord_y, cluster = dados$grupo)
  k <- max(dados$grupo)
  distancia = dist(matriz_dados)
  A = similaridade(matriz_dados)
  G = gauss_similaridade(A,1.0,0.0)
  gg = max(G)
  for (i in 1:length(G[,1])){
    G[i,i] <- 0
  }
  s=0
  Lf = familia_1(A)
  auto_cluster = dec_espectral(Lf)
  auto_cluster = round(auto_cluster, 6)
  km <- kmeans(auto_cluster, k, nstart=50)
  n_clusters <- max(km$cluster)
  n_clusters
  plot(df$x, df$y, col = km$cluster, pch=20, cex = 1)
  indices = 48+(k)
  km$cluster
 return(km)



}


