



#https://www.rprimer.dk/Rcode.html
library(MASS)

data("biopsy")

train <- sample(1:nrow(biopsy),ceiling(nrow(biopsy)*0.6))
testdata <- biopsy [-train,]
trainfit <- lda(class ~ V1 + V2 + V3 + V4 +V5 + V6 + V7 + V8 + V9,data = biopsy,subset = train)
print(trainfit)




 

result3<-sum(diag(result2)) / sum(result2)






print(result2)
print(result3)