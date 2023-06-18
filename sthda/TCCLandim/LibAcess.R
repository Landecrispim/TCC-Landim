library(TCCLandim)
require(igraph)
require(ggplot2)
require(ggmap)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization


#dados <- read.table("C:/Users/Lander/Desktop/", head = TRUE)
dados <- read.table("Data/polbooks.txt", head = TRUE)
k=2
s=0.2
R=7
#op <- par(mfrow = c(2, 2),las = 2)


a <- TCCLandim(dados,k,s,R)

#mat = matrix(nrow =34,ncol = 100 )
#a = TCCLandim(dados,k,4,R)

results = matrix()

df <- data.frame(i = numeric(), a = numeric())

for (i in 1:10000) {
  print(paste("Para i =", i/10000))
  a <- TCCLandim(dados, k, i/10000, R)$data$sil_width

  # Criar uma nova linha com os valores de i e a
  new_row <- data.frame(i = i/10000, a = a)

  # Adicionar a nova linha ao dataframe
  df <- rbind(df, new_row)
}

# Calcular a média de 'a' para cada valor de 'i'
media_a <- aggregate(a ~ i, data = df, FUN = mean)

# Criar o gráfico de linha
ggplot(media_a, aes(x = i, y = a)) +
  geom_line() +
  xlab("i") +
  ylab("Média de a")
#for (i in c(2:5)) {
 # a <- TCCLandim(dados,i,0.04,R)$cluster

  #mat[i,]<-a


#}



















