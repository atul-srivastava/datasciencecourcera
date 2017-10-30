Exercise Prediction

##Introduction The goal of this project is to predict the manner in which research participants exercised using data from an accelerometer device. Variables from the data sets were selected for analysis to predict the type of exercise. This report provides the following:

Describes how the predictive model was built.
How the model was cross validated.
Expected out of sample error claculation.
A prediction for 20 different test cases.
The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har.

##Data Processing

trainsrc <- read.csv("pml-training.csv")
testsrc <- read.csv("pml-testing.csv")
dim(trainsrc)
## [1] 19622   160
##Analysis

From the training data we identified the data set has 160 columns and 19622 rows. It is also noted that some of the collumns have no values. We will build a model based on columns with valid values and non identifying row columns.

## Remove Identify variables*
trainSet <- subset (trainsrc , select = -X: -num_window)
testSet <- subset (testsrc , select = -X: -num_window)


## Remove NA columns*
trainSet <- trainSet[, sapply(trainSet, function(x) !any(is.na(x)))]
trainSet <- trainSet[, sapply(trainSet, function(x) !any(x==""))]
testSet <- testSet[, sapply(testSet, function(x) !any(is.na(x)))]
## Warning: package 'caret' was built under R version 3.1.3
## Loading required package: lattice
## Loading required package: ggplot2
## Warning: package 'ggplot2' was built under R version 3.1.2
## Warning: package 'randomForest' was built under R version 3.1.3
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
*** Verify prediction model ***

In order to cross validate the model we will build two data sets and cross-validation. The model is built use the random forest model.

set.seed (77452)
trainIdx = createDataPartition(trainSet$classe, p = .75)[[1]]
training = trainSet[trainIdx,]
testing = trainSet[-trainIdx,]

## Build the prediction model

predictors <- training [, -53]
decision <- training [, 53]
rfMD <- randomForest (predictors, decision)
Expected Out of Samlple Error Claculation From the confusion matrix below the model is 99% accurate. The out of sample error is .35% .

predClass <- predict (rfMD, testing)
confusionMatrix (testing$classe,predClass)
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1395    0    0    0    0
##          B    3  945    1    0    0
##          C    0    4  851    0    0
##          D    0    0    4  800    0
##          E    0    0    0    5  896
## 
## Overall Statistics
##                                          
##                Accuracy : 0.9965         
##                  95% CI : (0.9945, 0.998)
##     No Information Rate : 0.2851         
##     P-Value [Acc > NIR] : < 2.2e-16      
##                                          
##                   Kappa : 0.9956         
##  Mcnemar's Test P-Value : NA             
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9979   0.9958   0.9942   0.9938   1.0000
## Specificity            1.0000   0.9990   0.9990   0.9990   0.9988
## Pos Pred Value         1.0000   0.9958   0.9953   0.9950   0.9945
## Neg Pred Value         0.9991   0.9990   0.9988   0.9988   1.0000
## Prevalence             0.2851   0.1935   0.1746   0.1642   0.1827
## Detection Rate         0.2845   0.1927   0.1735   0.1631   0.1827
## Detection Prevalence   0.2845   0.1935   0.1743   0.1639   0.1837
## Balanced Accuracy      0.9989   0.9974   0.9966   0.9964   0.9994
##Results The prediction model yielded the following predicted values for the 20 test cases.

##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
