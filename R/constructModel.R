####################################################################################
##                          CONSTRUCT PREDICTIVE MODEL
##
## This file will import the final cleaned data set, and initialize a model to 
## predict group membership.
####################################################################################

load(file = "data/analysisData.RData")

####################################################################################
## Define the fields to be used in training
####################################################################################
trainFields <- c("account_age",
                 "statuses_count",
                 "followers_count",
                 "favourites_count",
                 "friends_count",
                 "total_tweets",
                 "sentiment_republicans",
                 "affinity_republicans",
                 "emotion_republicans",
                 "user_id")
outcomeVar <- "troll"

####################################################################################
## Clean the data
####################################################################################
analysisData <- analysisData[,c(match(trainFields, names(analysisData)),
                                match(outcomeVar, names(analysisData)))]
analysisData <- na.omit(analysisData)

####################################################################################
## Transform text fields to lists of integers
####################################################################################
analysisData$emotion_republicans <- as.numeric(factor(analysisData$emotion_republicans))

####################################################################################
## Construct the training and test data sets
####################################################################################

## Identify the training sample
trainSample <- createDataPartition(y = analysisData[,outcomeVar], p=0.80, list=FALSE)

## Create a list to hold the training data
trainData = vector(mode="list", length = 2)
names(trainData)[1] <- "x"
names(trainData)[2] <- "y"

## Input the data sets into the training data vector
trainData[[1]] <- analysisData[trainSample, match(trainFields, names(analysisData))]
trainData[[2]] <- analysisData[trainSample, match(outcomeVar, names(analysisData))]

## Create a list to hold the test data
testData = vector(mode="list", length = 2)
names(testData)[1] <- "x"
names(testData)[2] <- "y"

## Input the data sets into the test data vector
testData[[1]] <- analysisData[-trainSample, match(trainFields, names(analysisData))]
testData[[2]] <- analysisData[-trainSample, match(outcomeVar, names(analysisData))]

## Create the final training data object
fullData = vector(mode="list", length = 2)
names(fullData)[1] <- "train"
names(fullData)[2] <- "test"

fullData[[1]] <- trainData
fullData[[2]] <- testData

####################################################################################
## Define the training and test data sets
####################################################################################

x_train <- fullData$train$x
y_train <- fullData$train$y
x_test <- fullData$test$x
y_test <- fullData$test$y

## Convert the training data sets into matrices
x_train <- as.matrix(x_train)
x_test <- as.matrix(x_test)

####################################################################################
## Format the data in preparation for training
####################################################################################

## The y data is an integer vector with values ranging from 0 to 1. To prepare this 
## data for training we one-hot encode the vectors into binary class matrices using 
## the Keras to_categorical() function:
y_train <- to_categorical(y_train, 2)
y_test <- to_categorical(y_test, 2)

####################################################################################
## Define the model
####################################################################################

model <- keras_model_sequential()
model %>% 
  layer_dense(units = ncol(x_train), input_shape = ncol(x_train)) %>% 
  layer_dropout(rate=0.4)%>%
  layer_activation(activation = 'relu') %>% 
  layer_dense(units = 2) %>% 
  layer_activation(activation = 'softmax')

summary(model)

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'adam',
  metrics = c('accuracy')
)

model %>% fit(
  x = x_train, 
  y = y_train, 
  epochs = 100, 
  batch_size = 32,
  validation_data = list(x_test, y_test)
)

####################################################################################
## BENCHMARK ACCURACY USING PROTOTYPE DATA: 0.9644
####################################################################################



