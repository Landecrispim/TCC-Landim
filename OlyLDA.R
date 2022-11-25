library(MASS)
train <- sample(1:nrow(athlete_events),ceiling(nrow(athlete_events)/3))
testdata <- athlete_events [-train,]
trainfit <- lda(Medal~ Age + Height + Weight ,data = athlete_events,subset = train)
print(trainfit)

