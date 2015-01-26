rm(list=ls())
getwd()
setwd("/Users/Jish-Book/Course/Getting and Cleaning Data/UCI HAR Dataset")
library("dplyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
library("reshape2", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
#reading files from the folders
trainData <- read.table(file = "./train/X_train.txt")
testData  <- read.table(file = "./test/X_test.txt")

#merging data from both files
totalData <- rbind(trainData,testData)

#reading data features.txt file
##labeling the data set with descriptive variable names.
featureData <- read.table(file = "features.txt")
featureData_names = as.character(featureData$V2)
names(totalData) = featureData_names

##indexing variables with measurements for mean and std for each measurement
meanIndex = grep(names(totalData),pattern = "mean",ignore.case = T)
stdIndex  = grep(names(totalData),pattern = "std",ignore.case = T)
allIndex<- c(meanIndex,stdIndex)
allIndex <- sort(allIndex)

#final data set after namings the variables
finalData <- totalData[,allIndex]

     
#Reading subject data for train and test
trainsubjectData <-  read.table(file = "./train/subject_train.txt")
testsubjectData <- read.table(file = "./test/subject_test.txt")
#combing data to original dataset
totalSubject <- rbind(trainsubjectData,testsubjectData)
finalData <- cbind(finalData,Subject= totalSubject$V1)

#Adding activity data to the dataset
trainactivityData <- read.table(file="./train/y_train.txt",sep="")
testactivityData  <- read.table(file="./test/y_test.txt",sep="")

# Combining Train and Test Data sets for variables
Total_activity_data <- rbind(trainactivityData,testactivityData )

head(finalData, n=1)

#Appropriately labels the data set with descriptive variable names. 

names_finalData = names(finalData) 

meanIndex  <- grep(names_finalData ,pattern = "mean")
stndIndex  <- grep(names_finalData ,pattern = "std",ignore.case = T)

new_labels <- gsub("\\)","",gsub("-","",gsub("mean","",gsub("std", "", names_finalData ))))
new_labels <- gsub("Y,","YaxisAnd",gsub("Z,","ZaxisAnd",gsub("X,","XaxisAnd",gsub("e(","eBetween",new_labels,fixed=TRUE),fixed=TRUE),fixed=TRUE),fixed=TRUE)
new_labels <- gsub("\\,","and",gsub("\\(","",new_labels))
new_labels[meanIndex] <- paste("MeanOf",new_labels[meanIndex],sep="")
new_labels[stndIndex] <- paste("StandardDeviationof",new_labels[stndIndex],sep="")

names(totalData) <- new_labels

Act_data <- read.table("./activity_labels.txt")
Act_data$V2 <- as.character(Act_data$V2)
for (idx in 1:length(Act_data$V1))
{
    Total_activity_data[Total_activity_data == idx] = Act_data$V2[idx]
}



finalData <- cbind(finalData,Activity = Total_activity_data$V1)
New_dataset <- melt(finalData, id.vars=c("Activity", "Subject"))
New_dataset_grouped <- group_by(New_dataset, Subject, Activity)
Result = summarise(New_dataset_grouped, mean=mean(value))

