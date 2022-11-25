
#https://www.rprimer.dk/Rcode.html
library(MASS)

data("iris")
library(klaR)
library(psych)
library(ggplot2)




set.seed(123)
ind <- sample(2, nrow(iris),
              replace = TRUE,
              prob = c(0.6, 0.4))
train <- iris[ind==1,]
testdata <- iris[ind==2,]


trainfit <-lda(Species~., train)
print(trainfit)
p <- predict(trainfit, testdata)
print = ldahist(data = p$x[,1], g = train$Species)
print(print)
#print(partimat(Species~., data = training, method = "lda"))
p1 <- predict(trainfit, testdata)$Species

tab <- table(Predicted = p1, Actual = testdata$class)

plot(trainfit,)


#print(sum(diag(tab))/sum(tab))





