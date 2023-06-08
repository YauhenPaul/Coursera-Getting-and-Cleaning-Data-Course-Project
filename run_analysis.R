#******************************************************************
#Step 0. Downloading, unzipping, and preparing dataset
#******************************************************************

filename <- "UCI_HAR_Dataset.zip"

# Checking if archieve already exists.
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method="curl")
}  

# Checking if folder exists
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}

# Assigning all data frames
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

#******************************************************************
#Step 1. Merges the training and the test sets to create one data set.
#******************************************************************

X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
Merged_Data <- cbind(Subject, Y, X)

#dim(Merged_Data)
#[1] 10299   563

#******************************************************************
#Step 2. Extracts only the measurements on the mean and standard deviation for each measurement.
#******************************************************************

TidyData <- Merged_Data %>% select(subject, code, contains("mean"), contains("std"))

#******************************************************************
#Step 3. Uses descriptive activity names to name the activities in the data set.
#******************************************************************

TidyData$code <- activities[TidyData$code, 2]

#******************************************************************
#Step 4. Appropriately labels the data set with descriptive variable names.
#******************************************************************

names(TidyData)[2] = "activity"
names(TidyData)<-gsub("Acc", "Accelerometer", names(TidyData))
names(TidyData)<-gsub("Gyro", "Gyroscope", names(TidyData))
names(TidyData)<-gsub("BodyBody", "Body", names(TidyData))
names(TidyData)<-gsub("Mag", "Magnitude", names(TidyData))
names(TidyData)<-gsub("^t", "Time", names(TidyData))
names(TidyData)<-gsub("^f", "Frequency", names(TidyData))
names(TidyData)<-gsub("tBody", "TimeBody", names(TidyData))
names(TidyData)<-gsub("-mean()", "Mean", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-std()", "STD", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-freq()", "Frequency", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("angle", "Angle", names(TidyData))
names(TidyData)<-gsub("gravity", "Gravity", names(TidyData))

#******************************************************************
#Step 5. From the data set in step 4, creating a second, independent tidy dataset with the average of each variable for each activity and each subject.
#******************************************************************

#5.1 Making a second tidy data set

final_dataset <- TidyData %>%
    group_by(subject, activity) %>%
    summarise_all(funs(mean))

#5.2 Writing second tidy dataset in txt file

write.table(final_dataset, "final_dataset.txt", row.name=FALSE)

