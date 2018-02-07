---
title: "Practical Machine Learning Peer Assessment"
author: "astaines"
date: "December 29, 2017"
output: 
  html_document: 
    keep_md: yes
---
## Introduction
### Background
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here:   
   
http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).

### Data

The training data for this project are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv


The data for this project come from this source: http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har

### Environment set up

```r
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)
library(corrplot)
library(knitr)
```

### Loading in the data
Next, we load in the data from the provided URLs:  

```r
trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
training <- read.csv(url(trainUrl), na.strings=c("NA","#DIV/0!",""))
testing <- read.csv(url(testUrl), na.strings=c("NA","#DIV/0!",""))
```

Now, we partition the training data into training and testing sets. The training set will account for 70% of the data and the testing set will account for 30%. The original testing data will remain untouched.

```r
in_train <- createDataPartition(y=training$classe, p=0.6, list=FALSE)
training_data <- training[in_train, ]; testing_data <- training[-in_train, ]
dim(training_data); dim(testing_data)
```

```
## [1] 11776   160
```

```
## [1] 7846  160
```

### Cleaning the data
First, we want to remove variables with near-zero variance: 

```r
NZV <- nearZeroVar(training_data)
training_data <- training_data[, -NZV]; testing_data  <- testing_data[, -NZV]
dim(training_data); dim(testing_data)
```

```
## [1] 11776   130
```

```
## [1] 7846  130
```

Then, we want to remove variables with a high percentage of NA values (95% or higher): 

```r
NAS   <- sapply(training_data, function(x) mean(is.na(x))) > 0.95
training_data <- training_data[, NAS==FALSE]; testing_data  <- testing_data[, NAS==FALSE]
dim(training_data); dim(testing_data)
```

```
## [1] 11776    59
```

```
## [1] 7846   59
```

Finally, we want to remove the ID variales, or the first 5 rows: 

```r
training_data <- training_data[, -(1:5)]; testing_data  <- testing_data[, -(1:5)]
dim(training_data); dim(testing_data)
```

```
## [1] 11776    54
```

```
## [1] 7846   54
```

## Choosing a Model: 
We will apply three methods for modeling this data: random forrest model, decision trees, and a generalized boosted model. The one with the highest accuracy will be used on the testing data set. 

### Random Forrest Model: 

```r
set.seed(12345)
random_forrest_control <- trainControl(method="cv", number=3, verboseIter=FALSE)
random_forrest_model <- train(classe ~ ., data=training_data, method="rf",
                          trControl=random_forrest_control)
random_forrest_model$finalModel
```

```
## 
## Call:
##  randomForest(x = x, y = y, mtry = param$mtry) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 27
## 
##         OOB estimate of  error rate: 0.3%
## Confusion matrix:
##      A    B    C    D    E class.error
## A 3348    0    0    0    0 0.000000000
## B    9 2266    4    0    0 0.005704256
## C    0    5 2046    3    0 0.003894839
## D    0    0    6 1922    2 0.004145078
## E    0    1    1    4 2159 0.002771363
```

Now, we will create a confusion matrix to test the accuracy of this model: 

```r
random_forrest_predict <- predict(random_forrest_model, newdata=testing_data)
random_forrest_confusion <- confusionMatrix(random_forrest_predict, testing_data$classe)
random_forrest_confusion
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 2231    6    0    0    0
##          B    0 1511    0    0    0
##          C    0    1 1368    9    0
##          D    0    0    0 1277    5
##          E    1    0    0    0 1437
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9972          
##                  95% CI : (0.9958, 0.9982)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9965          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9996   0.9954   1.0000   0.9930   0.9965
## Specificity            0.9989   1.0000   0.9985   0.9992   0.9998
## Pos Pred Value         0.9973   1.0000   0.9927   0.9961   0.9993
## Neg Pred Value         0.9998   0.9989   1.0000   0.9986   0.9992
## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
## Detection Rate         0.2843   0.1926   0.1744   0.1628   0.1832
## Detection Prevalence   0.2851   0.1926   0.1756   0.1634   0.1833
## Balanced Accuracy      0.9992   0.9977   0.9992   0.9961   0.9982
```

Finally, we will plot the confusion matrix results: 

```r
plot(random_forrest_confusion$table, col = random_forrest_confusion$byClass, main = paste("Random Forrest Confusion Matrix: Accuracy =", round(random_forrest_confusion$overall['Accuracy'], 4)))
```

![](Practical_Machine_Learning_Peer_Assessment_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

### Decision Tree: 

```r
set.seed(12345)
decision_tree_model <- rpart(classe ~ ., data=training_data, method="class")
fancyRpartPlot(decision_tree_model)
```

![](Practical_Machine_Learning_Peer_Assessment_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

Now, we will create a confusion matrix to test the accuracy of this model: 

```r
decision_tree_predict <- predict(decision_tree_model, testing_data, type = "class")
decision_tree_confusion <- confusionMatrix(decision_tree_predict, testing_data$classe)
decision_tree_confusion
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 2067  336   54  106   61
##          B   64  856   48   39  100
##          C   13  108 1100  185   85
##          D   51   96   74  721   58
##          E   37  122   92  235 1138
## 
## Overall Statistics
##                                           
##                Accuracy : 0.7497          
##                  95% CI : (0.7399, 0.7592)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.6813          
##  Mcnemar's Test P-Value : < 2.2e-16       
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9261   0.5639   0.8041  0.56065   0.7892
## Specificity            0.9008   0.9603   0.9396  0.95747   0.9241
## Pos Pred Value         0.7877   0.7733   0.7378  0.72100   0.7007
## Neg Pred Value         0.9684   0.9018   0.9578  0.91747   0.9511
## Prevalence             0.2845   0.1935   0.1744  0.16391   0.1838
## Detection Rate         0.2634   0.1091   0.1402  0.09189   0.1450
## Detection Prevalence   0.3344   0.1411   0.1900  0.12745   0.2070
## Balanced Accuracy      0.9134   0.7621   0.8719  0.75906   0.8566
```

Finally, we will plot the confusion matrix results: 

```r
plot(decision_tree_confusion$table, col = decision_tree_confusion$byClass, main = paste("Decision Tree Confusion Matrix: Accuracy =", round(decision_tree_confusion$overall['Accuracy'], 4)))
```

![](Practical_Machine_Learning_Peer_Assessment_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

### Generalized Boosted Model: 

```r
set.seed(12345)
GBM_control <- trainControl(method = "repeatedcv", number = 5, repeats = 1)
GBM_model  <- train(classe ~ ., data=training_data, method = "gbm",
                    trControl = GBM_control, verbose = FALSE)
```

```
## Warning: package 'gbm' was built under R version 3.3.3
```

```
## Loading required package: survival
```

```
## Warning: package 'survival' was built under R version 3.3.3
```

```
## 
## Attaching package: 'survival'
```

```
## The following object is masked from 'package:caret':
## 
##     cluster
```

```
## Loading required package: splines
```

```
## Loading required package: parallel
```

```
## Loaded gbm 2.1.3
```

```
## Warning: package 'plyr' was built under R version 3.3.3
```

```r
GBM_model$finalModel
```

```
## A gradient boosted model with multinomial loss function.
## 150 iterations were performed.
## There were 53 predictors of which 40 had non-zero influence.
```

Now, we will create a confusion matrix to test the accuracy of this model: 

```r
GBM_predict <- predict(GBM_model, newdata=testing_data)
GBM_confusion <- confusionMatrix(GBM_predict, testing_data$classe)
GBM_confusion
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 2219   14    0    0    0
##          B   13 1490    9    7    5
##          C    0   11 1350   22    2
##          D    0    2    8 1255   12
##          E    0    1    1    2 1423
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9861          
##                  95% CI : (0.9833, 0.9886)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9824          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9942   0.9816   0.9868   0.9759   0.9868
## Specificity            0.9975   0.9946   0.9946   0.9966   0.9994
## Pos Pred Value         0.9937   0.9777   0.9747   0.9828   0.9972
## Neg Pred Value         0.9977   0.9956   0.9972   0.9953   0.9970
## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
## Detection Rate         0.2828   0.1899   0.1721   0.1600   0.1814
## Detection Prevalence   0.2846   0.1942   0.1765   0.1628   0.1819
## Balanced Accuracy      0.9958   0.9881   0.9907   0.9863   0.9931
```

Finally, we will plot the confusion matrix results: 

```r
plot(GBM_confusion$table, col = GBM_confusion$byClass, main = paste("Generalized Boosted Model Confusion Matrix: Accuracy =", round(GBM_confusion$overall['Accuracy'], 4)))
```

![](Practical_Machine_Learning_Peer_Assessment_files/figure-html/unnamed-chunk-15-1.png)<!-- -->


## Conclusions
The accuracy of the 3 modeling methods are:
* Random Forest: 0.996
* Decision Tree: 0.709
* GBM: 0.987

Therefore, we choose to appy the Random Forrest Model to the testing dataset: 

```r
final_predict <- predict(random_forrest_model, newdata=testing)
final_predict
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```















