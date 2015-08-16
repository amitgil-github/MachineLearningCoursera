rm(list = ls()) #clean workspace
library(caret)

testing<- read.csv("pml-testing.csv", na.strings=c("NA"," ","#DIV/0!"))
training<- read.csv("pml-training.csv", na.strings=c("NA"," ","#DIV/0!"))

## clean rows with strange data
training<-training[training$new_window =="no",] 

#remove NA columns
training<-training[, colSums(is.na(training)) != nrow(training)]

#remove some more unneeded columns
training<-subset( training, select = -c(X,user_name,raw_timestamp_part_1,raw_timestamp_part_2,cvtd_timestamp,new_window))

# create training and test sets
inTrain <- createDataPartition(y=training$classe,p=0.7, list=FALSE)
traininggroup <- training[inTrain,]
testinggroup <- training[-inTrain,]

#Use Random Forest to estimate the result
tr1<-train(traininggroup$classe ~ ., data = traininggroup, method = "rf")

#check the result model Vs the internal testing group
confusionMatrix(testinggroup$classe, predict(tr1, testinggroup))

#predict test on the 20 cases in the testing data
predict(tr1, testing)

#find the most relevant parameters
varImp(tr1)
# and plot the relation between the most relevant ones
featurePlot(x=training[,c("num_window","roll_belt","yaw_belt","magnet_dumbbell_z")], y = training$classe,plot="pairs")
