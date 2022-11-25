data(biopsy)
biopsy[is.na(biopsy)] <- 0
my_pca <- prcomp(biopsy[2:10], scale = TRUE,
                 center = TRUE, retx = T)