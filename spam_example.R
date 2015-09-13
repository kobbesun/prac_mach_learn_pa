library(caret)
library(kernlab)
data(spam)

inTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)

training <- spam[inTrain,]
testing <- spam[-inTrain,]

dim(training)

set.seed(32343)

modelFit <- train(type ~., data = training, method = "glm")

predictions <- predict(modelFit, newdata = testing)

confusionMatrix(predictions, testing$type)

folds <- createFolds(y = spam$type, k = 10, list = TRUE, returnTrain = TRUE)

sapply(folds, length)

args(train.default)

args(trainControl)

library(ISLR)
library(ggplot2)
data("Wage")
summary(Wage)

inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training)
dim(testing)

featurePlot(x = training[, c("age", "education", "jobclass")], y = training$wage, plot = "pairs")


qplot(age, wage, data = training)

qplot(age, wage, color = jobclass, data = training)

qq <- qplot(age, wage, color = education, data = training)
qq + geom_smooth(method = "lm", formula = y ~ x)

library(Hmisc)
library(gridExtra)
cutWage <- cut2(training$wage, g = 3)
table(cutWage)

pl <- qplot(cutWage, age, data = training, fill = cutWage, geom = c("boxplot"))

p2 <- qplot(cutWage, age, data = training, fill = cutWage, geom = c("boxplot", "jitter"))

grid.arrange(pl, p2, ncol = 2)

t1 <- table(cutWage, training$jobclass)

prop.table(t1, 1)

qplot(wage, colour = education, data = training, geom = "density")


library(caret)
library(kernlab)

inTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

hist(training$capitalAve, main = "", xlab = "ave. capital run length")

preObj <- preProcess(training[,-58], method = c("center", "scale"))
transCapAveS <- predict(preObj, training[,-58])$capitalAve


set.seed(32343)
modelFit <- train(type ~ ., data = training, preProcess = c("center", "scale"), method = "glm")


preObj <- preProcess(training[, -58], method = c("BoxCox"))
trainCapAveS <- predict(preObj, training[, -58])$capitalAve
par(mfrow = c(1,2))
hist(trainCapAveS)
qqnorm(trainCapAveS)


set.seed(13343)
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1], size = 1, prob = 0.05) == 1
training$capAve[selectNA] <- NA

preObj <- preProcess(training[, -58], method = "knnImpute")
capAve <- predict(preObj, training[, -58])$capAve

capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth - mean(capAveTruth))/sd(capAveTruth)


quantile(capAve - capAveTruth)
quantile((capAve - capAveTruth)[selectNA])
quantile((capAve - capAveTruth)[!selectNA])

library(ISLR)
inTrain <- createDataPartition(y = Wage$wage, p = 0.7, list = FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

table(training$jobclass)

dummies <- dummyVars(wage ~ jobclass, data = training)
head(predict(dummies, newdata = training))

nsv <- nearZeroVar(training, saveMetrics = TRUE)

library(splines)
bsBasis <- bs(training$age, df = 3)

lml <- lm(wage ~ bsBasis, data = training)
plot(training$age, training$wage, pch = 19, cex = 0.5)
points(training$age, predict(lml, newdata = training), col = "red", pch = 19, cex = 0.5)

predict(bsBasis, age = testing$age)

library(caret)
library(kernlab)

inTrain <- createDataPartition(y = spam$type, p = 0.75, list = FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

M <- abs(cor(training[, -58]))
diag(M) <- 0
which(M > 0.8, arr.ind = T)

names(spam)[c(34, 32)]
plot(spam[, 34], spam[, 32])

X <- 0.71*training$num415 + 0.71*training$num857
Y <- 0.71*training$num415 - 0.71*training$num857
plot(X, Y)

