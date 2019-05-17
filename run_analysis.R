filePath <- "/Users/checsanctuary/"

setwd(filePath)
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip",method="curl")

###Unzip DataSet to /data directory
unzip(zipfile="./data/Dataset.zip",exdir="./data")

library(dplyr)
library(data.table)
library(tidyr)

filePath <- "/Users/checsanctuary/UCI HAR Dataset"
# Read subject file
SubjectTrain <- tbl_df(read.table(file.path(filePath, "train", "subject_train.txt")))
SubjectTest  <- tbl_df(read.table(file.path(filePath, "test" , "subject_test.txt" )))

# Read activity file
ActivityTrain <- tbl_df(read.table(file.path(filePath, "train", "Y_train.txt")))
ActivityTest  <- tbl_df(read.table(file.path(filePath, "test" , "Y_test.txt" )))

#Read data.
dataTrain <- tbl_df(read.table(file.path(filePath, "train", "X_train.txt" )))
dataTest  <- tbl_df(read.table(file.path(filePath, "test" , "X_test.txt" )))

#Below code will merge the training and the test sets by row binding and rename variables "subject" and "activityNum"

alldataSubject <- rbind(SubjectTrain,SubjectTest)
setnames(alldataSubject, "V1", "subject")
alldataActivity<- rbind(ActivityTrain, ActivityTest)
setnames(alldataActivity, "V1", "activityNum")

#combining training and test files
dataTable <- rbind(dataTrain, dataTest)

# name variables according to feature e.g.(V1 = "tBodyAcc-mean()-X")
dataFeatures <- tbl_df(read.table(file.path(filePath, "features.txt")))
setnames(dataFeatures, names(dataFeatures), c("featureNum", "featureName"))
colnames(dataTable) <- dataFeatures$featureName

#column names for activity labels
activityLabels<- tbl_df(read.table(file.path(filePath, "activity_labels.txt")))
setnames(activityLabels, names(activityLabels), c("activityNum","activityName"))

# Merge columns
alldataSubjAct<- cbind(alldataSubject, alldataActivity)
dataTable <- cbind(alldataSubjAct, dataTable)

# Read features and extract mean and standard deviation
dataFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",dataFeatures$featureName,value=TRUE) #var name

# Taking only measurements for the mean and standard deviation and add "subject","activityNum"
dataFeaturesMeanStd <- union(c("subject","activityNum"), dataFeaturesMeanStd)
dataTable<- subset(dataTable,select=dataFeaturesMeanStd)

dataTable <- merge(activityLabels, dataTable , by="activityNum", all.x=TRUE)
dataTable$activityName <- as.character(dataTable$activityName)

dataTable$activityName <- as.character(dataTable$activityName)
dataAggr<- aggregate(. ~ subject - activityName, data = dataTable, mean) 
dataTable<- tbl_df(arrange(dataAggr,subject,activityName))

head(str(dataTable),2)

names(dataTable)<-gsub("std()", "SD", names(dataTable))
names(dataTable)<-gsub("mean()", "MEAN", names(dataTable))
names(dataTable)<-gsub("^t", "time", names(dataTable))
names(dataTable)<-gsub("^f", "frequency", names(dataTable))
names(dataTable)<-gsub("Acc", "Accelerometer", names(dataTable))
names(dataTable)<-gsub("Gyro", "Gyroscope", names(dataTable))
names(dataTable)<-gsub("Mag", "Magnitude", names(dataTable))
names(dataTable)<-gsub("BodyBody", "Body", names(dataTable))
# Names after
head(str(dataTable),6)

write.table(dataTable, "TidyData.txt", row.name=FALSE)