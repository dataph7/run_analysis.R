## Load the required datasets
trainSet<- read.table("./train/X_train.txt")
trainLabel <- read.table("./train/y_train.txt")
trainSubject <- read.table("./train/subject_train.txt")
testSet <- read.table("./test/X_test.txt")
testLabel <- read.table("./test/y_test.txt")
testSubject <- read.table("./test/subject_test.txt")
actLabel <- read.table("./activity_labels.txt", stringsAsFactors = F)
features <- read.table("./features.txt")


## Merges the training and the test data to create one data set each
identical(names(trainSet), names(testSet)) ## Checking similarity
setMerge <- rbind(trainSet, testSet)
labelMerge <- rbind(trainLabel, testLabel)
subjectMerge <- rbind(trainSubject, testSubject)

## Extracts only the measurements on the mean and standard deviation
## for each measurement
colnames(setMerge) <- as.character(features[,2])
meanSet <- grep("mean()", colnames(setMerge), fixed = T)
sdSet <- grep("std()", colnames(setMerge), fixed = T)
meansd <- setMerge[, c(meanSet, sdSet)]

## Uses descriptive activity names to name the activities in the data set
labelMeansd <- cbind(labelMerge, meansd)
colnames(labelMeansd)[1] <- "Activity"

## Appropriately labels the data set with descriptive variable names
for(i in 1:length(labelMeansd[,1])){
        labelMeansd[i,1] <- actLabel[labelMeansd[i,1],2]
}

## From the data set in step 4, creates a second, independent tidy data set
## with the average of each variable for each activity and each subject.

dataSet <- cbind(subjectMerge, labelMeansd)
colnames(dataSet)[1] <- "Subject"
tidyData <- aggregate(dataSet[,3] ~ Subject+Activity, dataSet, "mean")
for(i in 4:ncol(dataSet)){
        tidyData[,i] <- aggregate(dataSet[,i] ~ Subject+Activity, dataSet, "mean")[,3]
}

colnames(tidyData)[3:ncol(tidyData)] <- colnames(meansd)


## Write the tidy data into file
write.table(tidyData, file = "tidy_data.txt" , row.name = F)



