inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
data(concrete)
install.packages("caret")
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
dim(testing)
install.packages("Hmisc")
library(Hmisc)

### cut2() function in the Hmisc package useful 
### for turning continuous covariates into factors

names <- colnames(concrete)
names <- names[-length(names)]
featurePlot(x = training[, names], y = training$CompressiveStrength, plot = "pairs")

index <- seq_along(1:nrow(training))
p <- ggplot(data = training, aes(x = index, y = CompressiveStrength))
p+ geom_point() + theme_bw()
cutCS <- cut2(training$CompressiveStrength, g = 4)
summary(cutCS)

ggplot(data = training, aes(y = index, x = cutCS)) + geom_boxplot() + geom_jitter(col = "blue") + 
  theme_bw()
featurePlot(x = training[, names], y = cutCS, plot = "box")



data(AlzheimerDisease)
adData = data.frame(diagnosis, predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[inTrain, ]
testing = adData[-inTrain, ]
IL_str <- grep("^IL", colnames(training), value = TRUE)
preProc <- preProcess(training[, IL_str], method = "pca", thresh = 0.8)
preProc$rotation

## make a subset of these predictors
predictors_IL <- predictors[, IL_str]
df <- data.frame(diagnosis, predictors_IL)
inTrain = createDataPartition(df$diagnosis, p = 3/4)[[1]]
training = df[inTrain, ]
testing = df[-inTrain, ]

## train the data using the first method
modelFit <- train(diagnosis ~ ., method = "glm", data = training)
predictions <- predict(modelFit, newdata = testing)
## get the confustion matrix for the first method
C1 <- confusionMatrix(predictions, testing$diagnosis)
print(C1)

## do similar steps with the caret package
modelFit <- train(diagnosis ~ ., method = "glm", preProcess = "pca", 
   data = training, trControl = trainControl(preProcOptions = list(thresh = 0.8)))
C2 <- confusionMatrix(testing$diagnosis, predict(modelFit, testing))
print(C2)


inTrain = createDataPartition(segmentationOriginal$Case, p = 0.6)[[1]]
training = segmentationOriginal[inTrain, ]
testing = segmentationOriginal[-inTrain, ]
set.seed(125)
modFit <- train(Case ~ ., method= "rpart", data = training)
print(modFit$finalModel)
