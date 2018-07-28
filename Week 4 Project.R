library(caret)

## Data Load / Input
URL1 = "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
URL2 = "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

training_raw = read.csv(url(URL1))
validation_raw = read.csv(url(URL2))

## Data Clean Up
# Remove Variables with zero variance
training_nzv = nearZeroVar(training_raw, saveMetrics = T)
training_raw2 = training_raw[,training_nzv$nzv== F]

# Remove Variables which are mostly NA, say 80%
Ind_NA = sapply(training_raw2, function(x) mean(is.na(x))) > 0.80
training_raw3 = training_raw2[, Ind_NA == F]

# Remove first 6 variables. They carry no info to prediction
training_raw4 = training_raw3[,-(1:6)]
head(training_raw4)
dim(training_raw4)

# Now we can create training and testing dataset
# Will continue to use 70/30 to split
inTrain = createDataPartition(y = training_raw4$classe, p = 0.7, list = F)
training = training_raw4[inTrain,] 
testing = training_raw4[-inTrain,]
dim(training)
head(training)

str(training)

## Start to Build Model on Traning Dataset
# Using 3-fold CV to select optimal tuning parameters
ctrl_rf = trainControl(method = "cv", number = 3, verboseIter = F)
modFit_rf = train(classe ~ ., data = training,
                  method = "rf", trControl = ctrl_rf, verbose = F)

# preProc = preProcess(log10(training[,-54]+1), method = "pca", pcaComp = 3)
modFit_rf$finalModel

confusionMatrix(testing$classe, predict(modFit_rf, newdata = testing))

## Model Validation
head(validation_raw)
# Validation dataset doesn't have classe variable

predict(modFit_rf, newdata = validation_raw)
# Result: B A B A A E D B A A B C B A E E A B B B