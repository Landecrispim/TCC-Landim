# Para manipular os dados
library(stats) # Para PCA
library(factoextra) # Para criar alguns gráficos
library(ggplot2)
data("iris")

dados_pca <- iris[,c(1:4)]

dados_pca[is.na(dados_pca)]<-0
sapply(dados_pca, sd)
pca_cov <- prcomp(dados_pca)
pca_corr <- prcomp(dados_pca, center = TRUE, scale = TRUE)
print = fviz_eig(pca_corr)
summary(pca_corr)$rotation



print = fviz_pca_biplot(pca_corr, repel = TRUE,
                        col.var = "black", # cor das variáveis
                        col.ind = as.factor(iris$Species),  
                        addEllipses = TRUE,
                        legend.title = "Species"
)


print2 = fviz_pca_var(pca_corr,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#print(print)
