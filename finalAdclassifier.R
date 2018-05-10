#loading the libraries 
library(dplyr)
library(lubridate)
library(caTools)
library(randomForest)
library(data.table)

#loading the data
train <-read.csv('train_sample.csv',stringsAsFactors = FALSE)
test <- fread("test.csv",header = T, stringsAsFactors = F, sep = ",")

#looking at the structure of test and train
str(train)
str(test)
#converting is_attributed(train set) in to factor
train$is_attributed=as.factor(train$is_attributed)



#checking if there are any missing values
colSums(is.na(train))
#seems like there were no missing values and given data is too clear 

#converting click_time in to date and time format
train$click_time<-as.POSIXct(train$click_time,
                             format = "%Y-%m-%d %H:%M",tz = "America/New_York")

####Feature Engineering 
#extracting year,month,weekdays,time
train$year=year(train$click_time)
train$month=month(train$click_time)
train$days=weekdays(train$click_time)
train$hour=hour(train$click_time)

#to check how many distinct values are there in each column
apply(train,MARGIN = 2, FUN = function(x) length(unique(x)))
#in data we have only for 1 year and 1 month so we can remove those coloumns

train$year <- NULL
train$month <- NULL
train$days <- NULL

#we can also remove the click_time since we already extracted the hour
train$click_time <- NULL

#before training the model removing the Y-Pred col 
#so making it null 
train$attributed_time <- NULL


#moving is_attributed to last col in train set before training the model
train <- train%>%select(-is_attributed,everything())


#splitting the train set in to training and testing set
set.seed(123)
split = sample.split(train$is_attributed,SplitRatio = 0.75)
training_set <- subset(train,split == TRUE)
testing_set <- subset(train,split == FALSE)


#Building the model using randomForest
library(e1071)
library(caret)
set.seed(678)
model <- train(x = training_set[,-7], y = training_set[,7], method = "rf", ntree = 100 ,trcontrol = trainControl(method = 'cv',number =10,verboseIter = TRUE))

#predicting the model on testing_set
model_predict <- predict(model,newdata = testing_set)

#confusion matrix 
table2 <- confusionMatrix(model_predict,testing_set$is_attributed)
table2

#Accuracy is 99% and sensitivity is 99%
#this model fits the data very well

##Now its time to apply the model on test data but before that 
#we need to make sure that test set is exact replicate of train set


#converting the click_time in to date and time format
test$click_time<-as.POSIXct(test$click_time,
                                 format = "%Y-%m-%d %H:%M",tz = "America/New_York")
 
 
test$hour=hour(test$click_time)
#now we can remove click_time
test$click_time <- NULL

 
#trying to add y_pred col to test set with value 0
test$is_attributed <- rep(0,nrow(test))
 
 
#taking out click_id from test set and creating a new DF for final output
finalprediction <- data.frame(click_id = test$click_id )

#now we can remove click_id from testData
test$click_id <- NULL
 

#checking the str of train and testData
str(train)
str(test)
#now both had same structures
#now the data set is all ready to apply algorithm


#Since my system can't support for applying the algorithm to test data set at same time
#Am dividing the test set  and also the finalprediction set in to 3 parts
tests <- split(test, rep(1:3, length.out = nrow(test), each = ceiling(nrow(test)/3)))
finalprediction <- split(finalprediction , rep(1:3, length.out = nrow(finalprediction), each = ceiling(nrow(finalprediction)/3)))

#applying the model on 3 test sets 
test_prediction1 <- predict(model,newdata = tests$'1')
test_prediction2 <- predict(model,newdata = tests$'2')
test_prediction3 <- predict(model,newdata = tests$'3')

#adding test_predictions to corresponding final_predictions
finalprediction$'1'$is_attributed  <- test_prediction1
finalprediction$'2'$is_attributed  <- test_prediction2
finalprediction$'3'$is_attributed  <- test_prediction3

#its time to make a final data frame with test set with corresponding predictions 
#row binding 
final <- rbind(finalprediction$`1`,finalprediction$`2`)
final <- rbind(final, finalprediction$`3`)

#Got a data frame with click_id and the predictions whether the user will download or not

#since its from kaggle we dont have exact answers 
# so just to check how many downloaded app or not 
table(final$is_attributed,test$is_attributed)

