# Para manipular os dados
library(stats) # Para PCA
library(factoextra) # Para criar alguns gráficos
library(ggplot2)


dados_pca <- biopsy[,c(2:10)]

dados_pca[is.na(dados_pca)]<-0
sapply(dados_pca, sd)
pca_cov <- prcomp(dados_pca)
pca_corr <- prcomp(dados_pca, center = TRUE, scale = TRUE)
print = fviz_eig(pca_corr)
summary(pca_corr)$rotation



print = fviz_pca_biplot(pca_corr, repel = TRUE,
                col.var = "black", # cor das variáveis
                col.ind = as.factor(biopsy$class),  # cor dos automoveis
                addEllipses = TRUE,
                legend.title = "Transmissão"
)



print(print)