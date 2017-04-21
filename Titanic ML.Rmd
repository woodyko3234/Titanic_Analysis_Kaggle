---
title: "Titanic ML"
author: "Woody Yao"
date: "2017/4/16"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, message = FALSE)
#Note that the `echo = FALSE` parameter was added to the code chunk to prevent 
#printing of the R code that generated the plot.
```


```{r}
# Set up for everything, load data
getwd()
titan_train <- read.csv('train.csv')
titan_test <- read.csv('test.csv')
test_ans <- read.csv('gender_submission.csv')


full  <- bind_rows(titan_train, titan_test)
#Import required libraries
library(ggplot2)
library(reshape2)
library(dplyr)
library(GGally)
library(gridExtra)
library(memisc)
library(scales)
library(nnet)
library(RColorBrewer)
library(randomForest)
```

##Data Dictionary

Variable	     Definition	              Key
survival	     Survival	                0 = No, 1 = Yes
pclass	       Ticket class            	1 = 1st, 2 = 2nd, 3 = 3rd
sex	           Sex	
Age	           Age in years	
sibsp	         # of siblings / spouses aboard the Titanic	
parch	         # of parents / children aboard the Titanic	
ticket	       Ticket number	
fare	         Passenger fare	
cabin	         Cabin number	
embarked	     Port of Embarkation	    C = Cherbourg, Q = Queenstown, 
                                        S = Southampton

###Variable Notes

pclass: A proxy for socio-economic status (SES)
1st = Upper
2nd = Middle
3rd = Lower

age: Age is fractional if less than 1. If the age is estimated, is it in the form of xx.5

sibsp: The dataset defines family relations in this way...
Sibling = brother, sister, stepbrother, stepsister
Spouse = husband, wife (mistresses and fiancés were ignored)

parch: The dataset defines family relations in this way...
Parent = mother, father
Child = daughter, son, stepdaughter, stepson
Some children travelled only with a nanny, therefore parch=0 for them.                                        
```{r}
# Check the formats of each variable
head(titan_train)
str(titan_train)
```

```{r}
# Turn some critical int into factor
titan_train$Survived <- factor(titan_train$Survived)
titan_train$Pclass <- factor(titan_train$Pclass)
```

```{r}
# Show simple distribution
summary(titan_train)
```

Note: Age info of 177 ppl in training dataset are unknown, which means we can 
      not apply it into our analysis. 
      Also, 2 ppl are without embarked location and 687 ppl are without cabin information.
```{r}
# Replace NA values
for (i in range(1, 891)){
  if (is.na(titan_train$Age[i])){
    titan_train$Age[i] = NULL
  }
  if (is.na(titan_train$Embarked[i])){
    titan_train$Embarked[i] = NULL
  }
}
```

## Univariate plotting

```{r}
ggplot(aes(x = SibSp), data = titan_train) +
  geom_bar() +
  geom_text(aes(y = (..count..), 
                label = ..count..), 
            stat = "count", vjust = -0.5, hjust = 0.5, check_overlap = T, 
            size = 3, position = position_dodge(width = 0))
```

```{r}
ggplot(aes(x = Parch), data = titan_train) +
  geom_bar() +
  geom_text(aes(y = (..count..), 
                label = ..count..), 
            stat = "count", vjust = -0.5, hjust = 0.5, check_overlap = T, 
            size = 3, position = position_dodge(width = 0))
```

```{r}
ggplot(aes(x = Pclass), data = titan_train) +
  geom_bar() +
  geom_text(aes(y = (..count..), 
                label = ..count..), 
            stat = "count", vjust = -0.5, hjust = 0.5, check_overlap = T, 
            size = 3, position = position_dodge(width = 0))
```

```{r}
table(titan_train$Ticket)
```

```{r}
# Does ticket type have any impact on suvival?
# https://www.ptt.cc/bbs/Statistics/M.1277714037.A.2CC.html
titan_train$ticket.type <- factor(substr(titan_train$Ticket, start = 1, stop = 1))

ggplot(aes(x = ticket.type, fill = Survived), data = titan_train) +
  geom_bar(position=position_dodge()) +
  geom_text(aes(y = (..count..), 
                label = ..count..), 
            stat = "count", vjust = -0.5, hjust = 0.5, check_overlap = T, 
            size = 3, position = position_dodge(width = 0.5))
```
Ticket type 1, 2, P have higher suvival rate than others

```{r}
# Does ticket fare have any impact on suvival?
# Cut the fare range

titan_train$fare.range <- cut(titan_train$Fare, 
                              c(-0.01, 5, 10, 15, 20, 25, 30, 50, 75, 100, 150, 
                                max(titan_train$Fare)+1))

ggplot(aes(fare.range, fill = Survived), data = titan_train)+
  geom_bar(position=position_dodge()) +
  geom_text(aes(y = (..count..), 
                label = ..count..), 
            stat = "count", vjust = -0.5, hjust = 0.5, check_overlap = T, 
            size = 3, position = position_dodge(width = 1))
```

Ticket fare higher than 50 got higher chance to survive

```{r}
# Other analysis on survival (Sex)
ggplot(aes(x = Sex, fill = Survived), data = titan_train) +
  geom_bar(aes(y = ((..count..)/sum(..count..))), position = 'dodge') +
  scale_y_continuous(labels = percent) +
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.5, hjust = 0, check_overlap = T, 
            size = 3, position = position_dodge(width = 0.7)) +
  ylab('Percentage') 

```
Females have much high chance to survive

```{r}
ggplot(aes(x = Pclass, fill = Survived), data = titan_train) +
  geom_bar(aes(y = ((..count..)/sum(..count..))), position = 'dodge') +
  scale_y_continuous(labels = percent) +
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.5, hjust = 0, check_overlap = T, 
            size = 3, position = position_dodge(width = 0.7)) +
  ylab('Percentage') 
```

Class 1 has pretty high chance to survive, 2 has about 50-50.

```{r}
# correlation with family and age
# chidren & women first?
ggplot(aes(x = Sex, fill = Survived), 
       data = subset(titan_train, Parch >= 1 & Age <= 25)) +
  geom_bar(position = 'dodge') +
  facet_wrap(~ Pclass) +
  geom_text(aes(y = (..count..), 
                label = ..count..), 
            stat = "count", vjust = -0.5, hjust = 0.5, check_overlap = T, 
            size = 3, position = position_dodge(width = 1))
```

Every woman with age below 25, boarding with family with ticket class 2 survived. Awesome!

```{r}
# Does embarking location matter?

ggplot(aes(x = Embarked, fill = Survived), data = titan_train) +
  geom_bar(position = 'dodge') + 
  facet_wrap(~ Pclass) +
  geom_text(aes(y = (..count..), 
                label = ..count..), 
            stat = "count", vjust = -0.5, hjust = 0.5, check_overlap = T, 
            size = 3, position = position_dodge(width = 1))
```

```{r}
# Try random forest
#install.packages("randomForest")
```
```{r}
str(titan_test)
```

```{r}
extractFeatures <- function(data) {
  features <- c("Pclass",
                "Age",
                "Sex",
                "Parch",
                "Ticket",
                "Fare",
                "Embarked")
  fea <- data[,features]
  fea$Age[is.na(fea$Age)] <- -1
  fea$Fare[is.na(fea$Fare)] <- 0
  fea$fare.range <- cut(fea$Fare, 
                              c(-0.01, 5, 10, 15, 20, 25, 30, 50, 75, 100, 150, 
                                515))
  fea$ticket.type <- factor(substr(fea$Ticket, start = 1, stop = 1))
  fea$Embarked[fea$Embarked==""] <- "S"
  fea$Sex      <- as.factor(fea$Sex)
  fea$Embarked <- as.factor(fea$Embarked)
  fea$Fare <- NULL
  fea$Ticket <- NULL
  return(fea)
}
# Keeping raising errors bcz structure of train does not equal to the one of test

# Create the forest.
#rf <- randomForest(extractFeatures(full), 
#                   as.factor(full$Survived), ntree = 100, 
#                   importance = TRUE)

# Write the analysis
#submission <- data.frame(PassengerId = titan_test$PassengerId)
#submission$Survived <- predict(rf, newdata = extractFeatures(titan_test))
#write.csv(submission, file = "1_random_forest_r_submission.csv", row.names=FALSE)


#imp <- importance(rf, type=1)
#featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])


# Importance of each predictor.
#print(importance(rf,type = 1)) 
```


```{r}
full$Age[is.na(full$Age)] <- -1
full$Fare[is.na(full$Fare)] <- 0
full$fare.range <- cut(full$Fare, 
                      c(-0.01, 5, 10, 15, 20, 25, 30, 50, 75, 100, 150, 
                        515))
full$ticket.type <- factor(substr(full$Ticket, start = 1, stop = 1))
full$Embarked[full$Embarked==""] <- "S"
full$Sex      <- as.factor(full$Sex)
full$Embarked <- as.factor(full$Embarked)
for (i in 1:1309){
  if (full$Parch[i] >0){
    if (full$Age[i] <= 18){
      full$Child[i] = TRUE
    }
  }
  else{full$Child[i] = FALSE}
}
```

```{r}
titan_train <- full[1:891,]
titan_test <- full[892:1309,]

rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + Parch + 
                                            fare.range + Embarked + ticket.type +
                                            Child,
                                            data = titan_train)
submission <- data.frame(PassengerId = titan_test$PassengerId)
submission$Survived <- predict(rf_model, newdata = titan_test)
write.csv(x = submission, file = 'randomForest2.csv', row.names = FALSE)
```
