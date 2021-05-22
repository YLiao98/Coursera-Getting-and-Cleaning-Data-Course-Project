## load library
library(reshape2)

## Retrieve data for the project
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
fileName <- "dataset.zip"

if(!file.exists(fileName)){
    download.file(fileUrl,fileName)
}

if(!file.exists("UCI HAR Dataset")){
    unzip(fileName)
}

## Merges the training and the test sets

# Read test folder data
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")

# Read train folder data
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")

# Combine features , subjects, labels into test and train tables
test <- cbind(X_test, subject_test, y_test)
train <- cbind(X_train, subject_train, y_train)

# Row-Combine two tables
combined_data <- rbind(train, test)

## Extracts only the measurements on the mean and standard deviation 
## for each measurement
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
features <- read.table("./UCI HAR Dataset/features.txt")

attr_lst <- c(as.character(features$V2),"subject","activity")

selected_cols <- grep("mean\\(\\)|std\\(\\)|subject|activity",attr_lst)

extracted_data <- combined_data[,selected_cols]

## Uses descriptive activity names to name the activities in the data set
names(activity_labels) <- c("activityId", "activityName")
extracted_data$V1.1 <- activity_labels$activityName[extracted_data$V1.1]


## Appropriately labels the data set with descriptive variable names. 
updatedNames <- attr_lst[selected_cols]    # Names after subsetting
updatedNames <- gsub("mean", "Mean", updatedNames)
updatedNames <- gsub("std", "Std", updatedNames)
updatedNames <- gsub("gravity", "Gravity", updatedNames)
updatedNames <- gsub("[[:punct:]]", "", updatedNames)
updatedNames <- gsub("^t", "time", updatedNames)
updatedNames <- gsub("^t", "time", updatedNames)
updatedNames <- gsub("BodyBody","Body",updatedNames)
updatedNames <- gsub("^f", "frequency", updatedNames)
updatedNames <- gsub("^anglet", "angleTime", updatedNames)

names(extracted_data) <- updatedNames

## creates a second, independent tidy data set 
## with the average of each variable for each activity and each subject.
library(dplyr)
tidyDataset <- extracted_data %>% 
    group_by(activity, subject) %>% 
    summarise_all(mean)

# Write tidy data to a file
write.table(tidyDataset, file = "tidy_data.txt", row.names = FALSE)
