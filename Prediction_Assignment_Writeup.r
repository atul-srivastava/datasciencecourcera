library(caret); library(tictoc); library(ggcorrplot)

train_raw <- read.csv("pml-training.csv", na.strings=c("NA","#DIV/0!",""))
testing <- read.csv("pml-testing.csv", na.strings=c("NA","#DIV/0!",""))

str(train_raw)

# use the column starting from 'roll_belt' & remove NAs
features <- names(testing[, colSums(is.na(testing)) == 0])[8:59]


train_raw <- train_raw[,c(features,"classe")]
testing <- testing[,c(features,"problem_id")]

dim(train_raw); dim(testing)

# check class imbalance 
table(train_raw$classe)

# missing values more than 90%
# mv <- lapply(train_raw, function(x) sum(is.na(x)/nrow(train_raw)))
# train_raw <- train_raw[, mv<=0.9]
# testing <- testing[, mv<=0.9]
# train_raw <- train_raw[, colSums(is.na(train_raw)) == 0] 
# testing <- testing[, colSums(is.na(testing)) == 0] 
# dim(train_raw); dim(testing)

# nearZeroVar
# nzv <- nearZeroVar(train_raw, freqCut = 95/5)
# print(nzv)
# train_raw <- train_raw[, -nzv]
# testing <- testing[, -nzv]
# dim(train_raw); dim(testing)

# find correlations and remove
nums <- sapply(train_raw, is.numeric)
highlyCorDescr <- findCorrelation(cor(train_raw[, nums], use="complete.obs"), cutoff = 0.9)
train_raw <- train_raw[, -highlyCorDescr]
testing <- testing[, -highlyCorDescr]
dim(train_raw); dim(testing)

# plot correlation 
nums <- sapply(train_raw, is.numeric)
corr <- round(cor(train_raw[, nums], use = "complete.obs"), 2)
ggcorrplot(corr, hc.order = TRUE, outline.col = "white") 


set.seed(12345)
inTrain <- createDataPartition(y=train_raw$classe, p=0.7, list=FALSE)

training <- train_raw[inTrain, ]
validation <- train_raw[-inTrain, ]

# check dimensions again
dflist <- list(train_raw, training, validation, testing)
sapply(dflist, dim)

# check class imbalance 
table(training$classe)

tic("ModelRF") # calculate time
setting <- trainControl(method="cv", classProbs=TRUE, 
                        savePredictions=TRUE,
                        allowParallel=TRUE, 
                        number = 10)
ModelRF <- train(classe ~ ., data=training, 
                 method="rf", 
                 trControl=setting, 
                 ntree=100)
toc()  # about 7 min
ModelRF

plot(ModelRF)

PredRF <- predict(ModelRF, validation)
Accuracy_RF <- confusionMatrix(PredRF, validation$classe)$overall['Accuracy']

confusionMatrix(PredRF, validation$classe)
RF_confuse <- as.matrix(confusionMatrix(PredRF, validation$classe))

# Plot Confusion Matrix
normalize <- function(m){
     (m - min(m))/(max(m)-min(m))
}

# normalize confusion matrix
input.matrix <- normalize(RF_confuse)


colnames(input.matrix) = c("A", "B", "C", "D", "E")
rownames(input.matrix) = colnames(input.matrix)

confusion <- as.data.frame(as.table(input.matrix))

plot <- ggplot(confusion)
plot + geom_tile(aes(x=Var1, y=Var2, fill=Freq)) + 
    scale_x_discrete(name="Actual Class") + 
    scale_y_discrete(limits = rev(levels(confusion$Var2)) ,name="Predicted Class") + 
    scale_fill_gradient(breaks=seq(from=-.5, to=4, by=.2)) + 
    labs(fill="Normalized\nFrequency") 



# choose C parameter for tuning
grid <- expand.grid(C = c(1, 100, 200, 300))

tic("ModelSVM") # calculate time
setting <- trainControl(method="cv", 5)
ModelSVM <- train(classe ~ ., data=training, 
                              method="svmLinear", 
                              trControl=setting, 
                              preProcess = c("center", "scale"),
                              tuneGrid = grid,
                              tuneLength = 10)
toc()  # about 2.4 hours
ModelSVM

plot(ModelSVM)

PredSVM <- predict(ModelSVM, validation)
Accuracy_SVM <- confusionMatrix(PredSVM, validation$classe)$overall['Accuracy']

confusionMatrix(PredSVM, validation$classe)
SVM_confuse <- as.matrix(confusionMatrix(PredSVM, validation$classe))

# Plot Confusion Matrix
normalize <- function(m){
     (m - min(m))/(max(m)-min(m))
}

# normalize confusion matrix
input.matrix <- normalize(SVM_confuse)


colnames(input.matrix) = c("A", "B", "C", "D", "E")
rownames(input.matrix) = colnames(input.matrix)

confusion <- as.data.frame(as.table(input.matrix))

plot <- ggplot(confusion)
plot + geom_tile(aes(x=Var1, y=Var2, fill=Freq)) + 
    scale_x_discrete(name="Actual Class") + 
    scale_y_discrete(limits = rev(levels(confusion$Var2)) ,name="Predicted Class (SVM)") + 
    scale_fill_gradient(breaks=seq(from=-.5, to=4, by=.2)) + 
    labs(fill="Normalized\nFrequency") 

Accuracy_RF ; Accuracy_SVM

PredRF_test <- predict(ModelRF,  testing)
PredSVM_test <- predict(ModelSVM, testing)

PredRF_test

PredSVM_test
