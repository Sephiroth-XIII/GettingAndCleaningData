#Download the data from given URL, save it as DATA.zip and unzip the file
URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if (!file.exists("getdata-projectfiles-UCI-HAR-Dataset.zip") && !file.exists("UCI HAR Dataset")) {
	download.file(URL, destfile = "getdata-projectfiles-UCI-HAR-Dataset.zip", method = "curl")
}
if (!file.exists("UCI HAR Dataset")) {
	unzip("getdata-projectfiles-UCI-HAR-Dataset.zip")
}

#Set working directory to unzipped data directory
setwd("UCI HAR Dataset")

#Read training data
X_train <- read.table("train/X_train.txt")
y_train <- read.table("train/y_train.txt")
subject_train <- read.table("train/subject_train.txt")

#Read test data
X_test <- read.table("test/X_test.txt")
y_test <- read.table("test/y_test.txt")
subject_test <- read.table("test/subject_test.txt")

#TASK 1 : Merge the training and the test sets to create one data set.
#SOLUTION : Since train and test data have same column names in same order, we append test data to train data to merge both datasets and create a new dataset.
X <- rbind(X_train, X_test)
y <- rbind(y_train, y_test)
subject <- rbind(subject_train, subject_test)

#TASK 2 : Extract only the measurements on the mean and standard deviation for each measurement.
#SOLUTION : Read features to know column names of the merged dataset X, extract column indexes with names having '-mean' and '-std' substrings in it, and then extract only those columns from X.
features <- read.table("features.txt")
indexes_to_be_extracted <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, indexes_to_be_extracted]

#TASK 3 : Uses descriptive activity names to name the activities in the data set.
#TASK 4 : Appropriately labels the data set with descriptive variable names.
#SOLUTION : get activity name from activity_labels.text and set those names instead of numbers in y dataset. Set currently extracted feature names to columns in X dataset
names(X) <- gsub("\\(|\\)", "", features[indexes_to_be_extracted, 2])
activities <- read.table("activity_labels.txt")
y[, 1] <- activities[y[, 1], 2]
names(y) <- "Activity"
names(subject) <- "Subject"

#TASK 5 : From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#SOLUTION: combine all three datasets column wise to form the tidydataset
tidy_data_set <- cbind(subject, y, X)
averaged_tidy_data_set <- tidyDataAVGSet <- aggregate(tidy_data_set[, 3 : dim(tidy_data_set)[2]] , list(tidy_data_set$Subject, tidy_data_set$Activity), mean)
names(averaged_tidy_data_set)[1] <- "Subject"
names(averaged_tidy_data_set)[2] <- "Activity"
names(averaged_tidy_data_set) <- gsub('Acc',"Acceleration",names(averaged_tidy_data_set))
names(averaged_tidy_data_set) <- gsub('GyroJerk',"AngularAcceleration",names(averaged_tidy_data_set))
names(averaged_tidy_data_set) <- gsub('Gyro',"AngularSpeed",names(averaged_tidy_data_set))
names(averaged_tidy_data_set) <- gsub('Mag',"Magnitude",names(averaged_tidy_data_set))
names(averaged_tidy_data_set) <- gsub('^t',"TimeDomain.",names(averaged_tidy_data_set))
names(averaged_tidy_data_set) <- gsub('^f',"FrequencyDomain.",names(averaged_tidy_data_set))
names(averaged_tidy_data_set) <- gsub('\\.mean',".Mean",names(averaged_tidy_data_set))
names(averaged_tidy_data_set) <- gsub('\\.std',".StandardDeviation",names(averaged_tidy_data_set))
names(averaged_tidy_data_set) <- gsub('Freq\\.',"Frequency.",names(averaged_tidy_data_set))
names(averaged_tidy_data_set) <- gsub('Freq$',"Frequency",names(averaged_tidy_data_set))

write.table(tidy_data_set, "tidy_data_set.txt", row.name=FALSE)
write.table(averaged_tidy_data_set, "averaged_tidy_data_set.txt", row.name=FALSE)

