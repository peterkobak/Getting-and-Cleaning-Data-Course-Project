## Requirement number 1
## Merges the training and the test sets to create one data set 

## load relevant libraries
library(data.table)
library(dplyr)


## bring in train data and bind together
xtrain <- read.table("train/X_train.txt")
ytrain <- read.table("train/y_train.txt")
trainsubject <- read.table("train/subject_train.txt")
trainbound <- cbind(trainsubject, ytrain, xtrain)

##bring in test data and bind together
xtest <- read.table("test/X_test.txt")
ytest <- read.table("test/y_test.txt")
testsubject <- read.table("test/subject_test.txt")
testbound <- cbind(testsubject, ytest, xtest)

## bind train and test data together
traintestbound <- rbind(trainbound, testbound)

##rename subject and activity columns
names(traintestbound)[1] <- "subject"
names(traintestbound)[2] <- "activity"

## Requirement number 4
## Appropriately labels the data set with descriptive variable names.

## read in features, remove numbered column from features
## and rename columns with feature names
features <- read.table("features.txt")
features <- as.character(features[,2])
names(traintestbound)[3:563] <- featureslist

## Requirement number # 2
## Extracts only the measurements on the mean and standard deviation
## for each measurement.

## remove first 2 rows
separate2 <- traintestbound[,1:2]
separate561 <-traintestbound[,3:563]

## extract only mean() and std()
extractedbound <-
  separate561[grepl("mean\\(\\)|std\\(\\)",names(separate561))]

## bring subject and activity columns back with extracted columns
bound <- cbind(separate2, extractedbound)


## Requirement number 3
## Uses descriptive activity names to name the activities in the data set

labels <- c("WALKING", "WALKINGUPSTAIRS", "WALKINGDOWNSTAIRS", "SITTING",
            "STANDING", "LAYING")

for (i in 1:6) {
  bound$activity <- gsub(i, labels[i], bound$activity)
}


## Requirement number 5
## From the data set in requirement 4, creates a second, independent tidy
## data set with the average of each variable for each activity and
## each subject.


## Setting subject as the factor value
bound$subject <- as.factor(bound$subject)
bound <- data.table(bound)

## Order entries in tidy and export it as a file "tidydata.txt"
tidy <- aggregate(. ~subject + activity, bound, mean)
tidy  <- tidy[order(tidy$subject,tidy$activity),]

write.table(tidy, file = "tidydata.txt", row.names = FALSE)

