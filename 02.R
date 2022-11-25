# Pacotes.
library(lattice)
library(latticeExtra)
library(MASS)
library(ellipse)
library(mvtnorm)

#-----------------------------------------------------------------------
# Usando um par de preditoras para visualizar a fronteira.

# Obtendo a matriz de covariância residual.
X <- as.matrix(subset(iris, select = c(Sepal.Length, Sepal.Width)))
Xs <- by(X,
         INDICES = iris$Species,
         FUN = scale,
         center = TRUE,
         scale = FALSE)
X <- do.call(rbind, Xs)
Sigma <- var(X)

# Dados centrados removendo efeito de Species, escala preservada.
xyplot(X[, 2] ~ X[, 1],
       groups = iris$Species,
       aspect = "iso") +
  layer(panel.ellipse(..., groups = NULL, col = 1))