
# This R script does the following:

# 1. Merges the training and the test sets to create one data set.

datax_train <- read.table("~/Documents/personal/Courses/3rd year/6th sem/Data Science john hopkins (PE2)/3.Getting and cleaning data/UCI HAR Dataset/train/X_train.txt")
datax_test <- read.table("~/Documents/personal/Courses/3rd year/6th sem/Data Science john hopkins (PE2)/3.Getting and cleaning data/UCI HAR Dataset/test/X_test.txt")
dataX <- rbind(datax_train, datax_test)

datay_train <- read.table("~/Documents/personal/Courses/3rd year/6th sem/Data Science john hopkins (PE2)/3.Getting and cleaning data/UCI HAR Dataset/train/y_train.txt")
datay_test <- read.table("~/Documents/personal/Courses/3rd year/6th sem/Data Science john hopkins (PE2)/3.Getting and cleaning data/UCI HAR Dataset/test/y_test.txt")
dataY <- rbind(datay_train, datay_test)

datasub_train <- read.table("~/Documents/personal/Courses/3rd year/6th sem/Data Science john hopkins (PE2)/3.Getting and cleaning data/UCI HAR Dataset/train/subject_train.txt")
datasub_test <- read.table("~/Documents/personal/Courses/3rd year/6th sem/Data Science john hopkins (PE2)/3.Getting and cleaning data/UCI HAR Dataset/test/subject_test.txt")
dataSubject <- rbind(datasub_train, datasub_test)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

datafeatures <- read.table("~/Documents/personal/Courses/3rd year/6th sem/Data Science john hopkins (PE2)/3.Getting and cleaning data/UCI HAR Dataset/features.txt")
mean_sd_features_index <- grep("-mean\\(\\)|-std\\(\\)", datafeatures[, 2])
dataX <- dataX[, mean_sd_features_index]
names(dataX) <- datafeatures[mean_sd_features, 2]
names(dataX) <- gsub("\\(|\\)", "", names(dataX))
names(dataX) <- tolower(names(dataX))

# 3. Uses descriptive activity names to name the activities in the data set.

activity <- read.table("~/Documents/personal/Courses/3rd year/6th sem/Data Science john hopkins (PE2)/3.Getting and cleaning data/UCI HAR Dataset/activity_labels.txt")
activity[, 2] = gsub("_", "", tolower(as.character(activity[, 2])))
dataY[, 1] = activity[dataY[ , 1], 2]
names(dataY) <- "activity"

# 4. Appropriately labels the data set with descriptive activity names.

names(dataSubject) <- "subject"
clean <- cbind(dataSubject, dataY, dataX)
write.table(clean, "merged_clean_and_tidy_data.txt")

# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects <- unique(dataSubject)[, 1]
numSubjects <- length(unique(dataSubject)[, 1])
numActivities <- length(activity[, 1])
numColumns <- dim(clean)[2]
result <- clean[1:(numSubjects*numActivities), ]

row <- 1
for (s in 1:numSubjects){
  for (a in 1:numActivities){
    result[row, 1] <- uniqueSubjects[s]
    result[row, 2] <- activity[a, 2]
    temp <- clean[clean$subject == s & clean$activity == activity[a, 2], ]
    result[row, 3:numColumns] <- colMeans(temp[, 3:numColumns])
    row <- row + 1
  }
}
write.table(result, "data_set_with_the_averages.txt")
