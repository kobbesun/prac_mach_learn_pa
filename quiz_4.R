set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)
library(elasticnet)
lasso.model <- train(CompressiveStrength~., data=training, method="lasso")
plot.enet(lasso.model$finalModel, xvar="penalty", use.color=TRUE)

library(lubridate)  # For year() function below
dat = read.csv("./gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
library(forecast)

mod <- bats(tstrain)
fcast <- forecast.bats(mod, level=95, h=nrow(testing))
acc <- accuracy(fcast, testing$visitsTumblr)

count <- 0
for (i in 1:nrow(testing)) {
        if (testing$visitsTumblr[i] > fcast$lower[i]) {
                if(testing$visitsTumblr[i] < fcast$upper[i]) {
                        count <- count + 1}
        }
}
count/nrow(testing)

q2Fit <- bats(tstrain)
forecast(q2Fit)

set.seed(3523)
library(AppliedPredictiveModeling)
library(caret)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

library(e1071)
set.seed(325)

q3SVM <- svm(CompressiveStrength ~ ., data = training)
q3SVMPredict <- predict(object = q3SVM, newdata = testing)
accuracy(f = q3SVMPredict, x = testing$CompressiveStrength)


set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

require(e1071)
set.seed(325)
fit<-svm(CompressiveStrength ~., data = training )
pred<-predict(fit, testing)
accuracy(f = pred, x = testing$CompressiveStrength)

data(Glass, package="mlbench")
## split data into a train and test set
index <- 1:nrow(Glass)
testindex <- sample(index, trunc(length(index)/3))
testset <- Glass[testindex,]
trainset <- Glass[-testindex,]

svm.model <- svm(Type ~ ., data = trainset, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, testset[,-10])
