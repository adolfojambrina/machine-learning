---
title: "machine learning project"
author: "Adolfo Jambrina"
date: "2024-01-06"
output: html_document
---



## Introduction

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: 
<http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har>

The training data for this project are available here:

<https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv>

The test data are available here:

<https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv>



## Data Preprocessing

```{r setup}
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(cache=TRUE)
setwd("~/machine_learning/project")
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(corrplot)
```

## Download Data

```{r get data}
url1<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
url2<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
data_train<-download.file(url1,"pml-training.csv",method="curl")
data_test<-download.file(url2,"pml-testing.csv",method="curl")
train<-read.csv("pml-training.csv",sep=",")
test<-read.csv("pml-testing.csv",sep=",")
dim(train)
dim(test)
```
## Clean Data
In this step, we will clean the data and get rid of observations with missing calues and meaningless variables.
```{r}
sum(complete.cases(train))
```
Remove columns with NAs values
```{r NA}
train_na<-train[,colSums(is.na(train))==0]
test_na<-train[,colSums(is.na(test))==0]
```
Next, we get rid of some columns that no contribute much to accelerometer measurements.
```{r remove}
classe<-train_na$classe
train_rm<-grepl("^X|timestamp|window",names(train_na))
train_na<-train_na[,!train_rm]
train_clean<-train_na[,sapply(train_na,is.numeric)]
train_clean$classe<-classe

test_rm<-grepl("^X|timestamp|window",names(test_na))
test_na<-test_na[,!test_rm]
test_clean<-test_na[,sapply(test_na,is.numeric)]

```

## Slice data
Then, we can split the cleaned training set into a pure training data set (70 %) and a validation data set (30 %). We will use the validation data set to conduct cross validation in future steps
```{r slice}
set.seed(1234)
inTrain<-createDataPartition(train_clean$classe, p=0.70, list=FALSE)
trainData<-train_clean[inTrain,]
testData<-train_clean[-inTrain,]

```

## Data Modeling
We fit a predictive model for activity recognition using Random Forest algorithm because it automatically selects important variables and is robust to correlated covariates & autliers in general. We will use **5-fold cross validation** when applying the algorithm.

```{r}
controlRf<-trainControl(method="cv",5)
modelRf<-train(classe~.,data=trainData,method="rf",trControl=controlRf,ntree=250)
modelRf
```

Then, we estimate the performance of the model on the validation data set.
```{r porformance}
predictRf<-predict(modelRf,testData)
confusionMatrix(table(testData$classe,predictRf))
```
The stimated out-of-sample error is: 
```{r oose}
oose<-1-as.numeric(confusionMatrix(table(testData$classe,predictRf))$overall[1])
oose
```

So, the estimated accuracy of the model is 99.39 % and the stimated out-of-sample error is 0.61 %



## Appendix: Figures
1. Correlation Matrix Visualization
```{r}
corrPlot<-cor(trainData[,-length(names(trainData))])
corrplot(corrPlot,method="color")
```

2. Decision Tree Visualization
```{r}
treeModel <- rpart(classe ~ ., data=trainData, method="class")
prp(treeModel)
```

