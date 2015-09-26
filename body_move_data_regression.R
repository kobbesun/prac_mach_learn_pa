library(caret)

dfExam <- read.csv("./data/pml-testing.csv", na.strings=c("", "NA", "NULL", "#DIV/0!"))
dfSample <- read.csv("./data/pml-training.csv", na.strings=c("", "NA", "NULL", "#DIV/0!"))

# assess data points
summary(dfSample)

# remove data points with too many NAs
dfSamplePro <- dfSample[ , colSums(is.na(dfSample)) < 200]

# remove irrelevant data points
dfSamplePro$X <- NULL
dfSamplePro$user_name <- NULL
dfSamplePro$raw_timestamp_part_1 <- NULL
dfSamplePro$raw_timestamp_part_2 <- NULL
dfSamplePro$cvtd_timestamp <- NULL

# slice data into train, validation, and test
vInTrainValid <- createDataPartition(y = dfSamplePro$classe, p = 0.7, list = FALSE)
dfTrainValid <- dfSamplePro[vInTrainValid,]
dfTesting <- dfSamplePro[-vInTrainValid,]

vInTrain <- createDataPartition(y = dfTrainValid$classe, p = 0.8, list = FALSE)
dfTrain <- dfTrainValid[vInTrain,]
dfValid <- dfTrainValid[-vInTrain,]

# try Rpart model
modRpart <- train(classe ~ ., method = 'rpart', data = dfTrain)
valRpart <- predict(modRpart, dfValid)
table(valRpart, dfValid$classe)

# adjust Rpart model
modRpart2 <- rpart(classe ~ ., data = dfTrain, 
                   control = rpart.control(maxcompete = 40, maxsurrogate = 50, maxdepth = 30))
valRpart2 <- predict(modRpart2, dfValid, type = 'class')
table(valRpart2, dfValid$classe)

# try radom forest model
library(randomForest)
set.seed(222)
modRf <- randomForest(classe ~ ., data = dfTrain, ntree = 100, importance = TRUE)
valRf <- predict(modRf, dfValid, type = 'class')
table(valRf, dfValid$classe)

preRf <- predict(modRf, dfTesting, type = 'class')
table(preRf, dfTesting$classe)


# generate prediction
dfExamPre <- subset(dfExam, select = colnames(dfTesting)[1:54])
#dfExamPre$classe <- NULL
#levels(dfExamPre$new_window) <- levels(dfTesting$new_window)
levels(dfExam$new_window) <- levels(dfTesting$new_window)
examRf <- predict(modRf, dfExam, type = 'class')

# export result
pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}

pml_write_files(examRf)
