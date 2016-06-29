# Loading test.csv and train.csv 
test = read.csv("test2016.csv")
train = read.csv("train2016.csv")

#Observing the structure of train 
str(train)

#Removing unanswered questions with Possible/Expected answer of Gender and Age
summary(train$Gender)
train$Gender = as.character(train$Gender)
train$Gender[train$Gender == ""] = "Unspecified"
train$Gender = as.factor(train$Gender)
summary(train$Gender)

summary(test$Gender)
test$Gender = as.character(test$Gender)
test$Gender[test$Gender == ""] = "Unspecified"
test$Gender = as.factor(test$Gender)
summary(test$Gender)

summary(train$Age)
train$Age = 2016 - train$YOB
train$Age[is.na(train$Age) == TRUE] = median(train$Age[is.na(train$Age) == FALSE])
train$Age[train$Age < 18] = median(train$Age[is.na(train$Age) == FALSE])
train$Age[train$Age > 75] = median(train$Age[is.na(train$Age) == FALSE])
summary(train$Age)

summary(test$Age)
test$Age = 2016 - test$YOB
test$Age[is.na(test$Age) == TRUE] = median(test$Age[is.na(test$Age) == FALSE])
test$Age[test$Age < 18] = median(test$Age[is.na(test$Age) == FALSE])
test$Age[test$Age > 75] = median(test$Age[is.na(test$Age) == FALSE])
summary(test$Age)

#Loading library(caTools) to split the available training data into further two parts for Training and Testing
library(caTools)
split = sample.split(train$Party,SplitRatio=0.6)
Train = subset(train,split==TRUE)
Test = subset(train,split==FALSE)

#Using Logistic Model with relevant Variables after Observing Significance from summary and Reading Questions from pdf
model = glm(Party ~ Gender + Age + HouseholdStatus + Income + EducationLevel + Q98197 + Q98869 + Q99982 + Q101162 + Q102089 + Q102674 + Q106042 + Q108342 + Q109367 + Q109244 + Q108950 + Q108754 + Q113181 + Q113583 + Q113992 + Q113584 + Q114152 + Q114748 + Q114961 + Q115195 + Q115611 + Q115610 + Q115899 + Q116953 + Q118232 + Q119650 + Q120012 + Q120194 + Q121699 + Q123464 + Q123621,data=Train,family="binomial")
predTest = predict(model,newdata=Test,type="response")
PredTest = predict(model, newdata=test, type="response")

#Initialising General Threshold value
threshold = 0.5

#Generating .csv file to submit in Kaggle
PredTestLabels = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))
MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)
write.csv(MySubmission, "SubmissionVoting.csv", row.names=FALSE)