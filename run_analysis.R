## load required packages
library(data.table)
library(dplyr)

## extract names and labels from files
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = F)
activity_Test <- read.table("UCI HAR Dataset/test/y_test.txt", header = F)
activity_Train <- read.table("UCI HAR Dataset/train/y_train.txt", header = F)
featureNames <- read.table("UCI HAR Dataset/features.txt")
features_Test <- read.table("UCI HAR Dataset/test/X_test.txt", header = F)
features_Train <- read.table("UCI HAR Dataset/train/X_train.txt", header = F)
subject_Test <- read.table("UCI HAR Dataset/test/subject_test.txt", header = F)
subject_Train <- read.table("UCI HAR Dataset/train/subject_train.txt", header = F)

## Merge rows for train and test tables
activity <- rbind(activity_Train, activity_Test)
features <- rbind(features_Train, features_Test)
subject <- rbind(subject_Train, subject_Test)

## Name columns and apply to new table
colnames(activity) <- "Activity"
colnames(features) <- t(featureNames[2])
colnames(subject) <- "Subject"
allData <- cbind(features,activity,subject)

## Extract columns with either Mean or SD
columns_MeanSD <- grep(".*Mean.*|.*Std.*", names(allData), ignore.case=T)
NeededColumns <- c(columns_MeanSD, 562, 563)
extractData <- allData[,NeededColumns]

## Change activity to character, and then to factor type
extractData$Activity <- as.character(extractData$Activity)
for (i in 1:6){
  extractData$Activity[extractData$Activity == i] <- as.character(activityLabels[i,2])}
extractData$Activity <- as.factor(extractData$Activity)

## Change names to descriptive names
names(extractData)<-gsub("Acc", "Accelerometer", names(extractData))
names(extractData)<-gsub("Gyro", "Gyroscope", names(extractData))
names(extractData)<-gsub("BodyBody", "Body", names(extractData))
names(extractData)<-gsub("Mag", "Magnitude", names(extractData))
names(extractData)<-gsub("^t", "Time", names(extractData))
names(extractData)<-gsub("^f", "Frequency", names(extractData))
names(extractData)<-gsub("tBody", "TimeBody", names(extractData))
names(extractData)<-gsub("-mean()", "Mean", names(extractData), ignore.case = T)
names(extractData)<-gsub("-std()", "STD", names(extractData), ignore.case = T)
names(extractData)<-gsub("-freq()", "Frequency", names(extractData), ignore.case = T)
names(extractData)<-gsub("angle", "Angle", names(extractData))
names(extractData)<-gsub("gravity", "Gravity", names(extractData))

## Set subject as factor
extractData$Subject <- as.factor(extractData$Subject)
extractData <- data.table(extractData)

## Create file TidyDataFile with Mean values
finalData <- aggregate(. ~Subject + Activity, extractData, mean)
finalData <- finalData[order(finalData$Subject,finalData$Activity),]
write.table(finalData, file = "TidyDataFile.txt", row.names = F)
