---
title: "Prediction Assignment - Coursera"
author: "Frederick Orndorff"
date: "12/9/2019"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Introduction

It is now possible to collect a large amount of data about personal
movement using activity monitoring devices such as a
[Fitbit](http://www.fitbit.com), [Nike
Fuelband](http://www.nike.com/us/en_us/c/nikeplus-fuelband), or
[Jawbone Up](https://jawbone.com/up). These type of devices are part of
the "quantified self" movement -- a group of enthusiasts who take
measurements about themselves regularly to improve their health, to
find patterns in their behavior, or because they are tech geeks. But
these data remain under-utilized both because the raw data are hard to
obtain and there is a lack of statistical methods and software for
processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012
and include the number of steps taken in 5 minute intervals each day.

## Data

The data for this assignment can be downloaded from the course web
site:

* Dataset: [Activity monitoring data](http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har)
* [Training Set](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv)
* [Testing Set](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv)

The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 19,622 observations in the training set and the testing
set includes 20 observations dataset.

```{r}
#Load Packages and import data
library(caret); library(randomForest);
library(rpart); library(rpart.plot)

#Make sure the working directory is where the *.csv files are located
training <- read.csv("pml-training.csv", na.strings = c("NA", ""))
testing <- read.csv("pml-testing.csv", na.strings = c("NA", ""))
```

## Data Processing (Cleaning)
Before we start the prediction we should ensure the data is not missing any values and is in the correct format.

```{r}
training <- training [, colSums(is.na(training)) == 0]
testing <- testing [, colSums(is.na(testing)) == 0]
```

# Create a validation set

I am unsure if there is a need for a validation set - but this is what was taught in class, so the code
below creates a validation set of 30 percent of the testing set.

```{r}
set.seed(1111)
training_2 <- createDataPartition(training$classe, p = 0.7, list = FALSE)
train_set <- training[training_2,]
validation_set <- training[-training_2,]
```

## Prediction Models

Lets start with classification trees and random forests to predict the class of exercise.

# Classification Trees
```{r}
class_train <- trainControl(method = 'cv', number = 10)
fit_rpart <- train(classe ~., data = train_set, method = 'rpart')
print(fit_rpart, digits = 3)
```

Lets see a plot:

```{r}
library(rattle)
fancyRpartPlot(fit_rpart$finalModel)
```

# Confusion Matrix:

```{r}
predict_rpart <- predict(fit_rpart, validation_set)
print (cm_rpart <- confusionMatrix(validation_set$classe, predict_rpart))
```

```{r}
print (acc_rpart <- cm_rpart$overall[1])
```

A 66% accuracy is alright - but can we do better with Random Forests??

## Random Forests

```{r}
#This takes too long
#fit_rf <- train(classe ~., data = train_set, method = 'rf')
#lets try something else
fit_rf <- randomForest(classe ~., data = train_set)
print (fit_rf, digits = 3)
```
WOW - that took FOREVER to finish... maybe it is time to upgrade the computer??

Lets check the confusion matrix and the overall accuracy:

```{r}
predict_rf <- predict(fit_rf, validation_set)
print (confusion_rf <- confusionMatrix(validation_set$classe, predict_rf))
print (accuracy_rf <- confusion_rf$overall[1])
```

It seems like the random forests provides a better prediction accuracy -- but it cost me may minutes of
surfing the web -- waiting for the computations to be completed.  The last step is to test the model(s)
on the test set.

## Final Prediction
```{r}
Final_pred <- predict(fit_rf, data = testing)
Final_pred
```

