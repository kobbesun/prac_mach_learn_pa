library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

testing <- subset(segmentationOriginal, Case == "Test")
training <- subset(segmentationOriginal, Case == "Train")
set.seed(125)
q1ModFit <- train(Class ~ ., method = "rpart", data = training)
predict(q1ModFit, TotalIntench2 = 23,000, FiberWidthCh1 = 10)

library(pqmm)
data(olive)
olive2 <- olive[, -1]

newdata <- as.data.frame(t(colMeans(olive2)))
q2ModFit <- train(Area ~ ., method = "rpart", data = olive2)
predict(q2ModFit, newdata = newdata)
predict(q2ModFit, newdata = olive2[2,])

library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
set.seed(13234)

q3ModFit <- glm(chd ~ age + alcohol + obesity + tobacco + typea + ldl, family = binomial(), data = trainSA)
q3ModFit <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, method = "glm", family = "binomnial", data = trainSA)

q3Predict <- predict(q3ModFit, testSA)
q3PredictTrain <- predict(q3ModFit, trainSA)
missClass(testSA$chd, q3Predict)
missClass(trainSA$chd, q3PredictTrain)

library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)
q5ModFit <- train(y ~ ., data = vowel.train, method = "rf", prox = TRUE)
varImp(q5ModFit, scale = FALSE)