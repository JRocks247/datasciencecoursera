library(plyr)
setwd("C:/Users/Jono/Desktop/Coursera/Getting and Cleaning Data/UCI HAR Dataset/")

# Question 1 - Merge the training and the test sets to create one data set.

  # Load the data and assign a row number as the values of ID column

subject_train = read.table('./train/subject_train.txt', col.names=c("subject_id"))  
X_train       = read.table('./train/X_train.txt')
y_train       = read.table('./train/y_train.txt', col.names=c("activity_id"))
subject_test  = read.table('./test/subject_test.txt', col.names=c("subject_id"))
X_test        = read.table('./test/X_test.txt')
y_test        = read.table('./test/y_test.txt', col.names=c("activity_id"))
features      = read.table('./features.txt', col.names=c("feature_id", "feature_label"),)
activity_labels = read.table('./activity_labels.txt', col.names=c("activity_id", "activity_label"),)

    # read the data

subject_train$ID  <- as.numeric(rownames(subject_train))
X_train$ID        <- as.numeric(rownames(X_train))
y_train$ID        <- as.numeric(rownames(y_train))
subject_test$ID   <- as.numeric(rownames(subject_test))
X_test$ID         <- as.numeric(rownames(X_test))
y_test$ID         <- as.numeric(rownames(y_test))

      # merge all train and test then bind

train <- merge(subject_train, y_train, all=TRUE)
train <- merge(train, X_train, all=TRUE)
test  <- merge(subject_test, y_test, all=TRUE) 
test  <- merge(test, X_test, all=TRUE)

Question1 <- rbind(train, test)

# Question 2 - Extract only the measurements on the mean and standard deviation for each measurement.

selected_features <- features[grepl("mean\\(\\)", features$feature_label) | grepl("std\\(\\)", features$feature_label), ]

Question2 <- Question1[, c(c(1, 2, 3), selected_features$feature_id + 3) ]

# Question 3 - Use descriptive activity names to name the activities in the data set

Question3 = merge(Question2, activity_labels)

# Question 4 - Appropriately label the data set with descriptive variable names.

selected_features$feature_label = gsub("\\(\\)", "", selected_features$feature_label)
selected_features$feature_label = gsub("-", ".", selected_features$feature_label)

for (i in 1:length(selected_features$feature_label)) {
  colnames(Question3)[i + 3] <- selected_features$feature_label[i]
}
Question4 = Question3

# Question 5 -  From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.

drops     <- c("ID","activity_label")
Question5 <- Question4[,!(names(Question4) %in% drops)]
aggdata   <-aggregate(Question5, by=list(subject = Question5$subject_id, activity = Question5$activity_id), FUN=mean, na.rm=TRUE)
drops     <- c("subject","activity")
aggdata   <- aggdata[,!(names(aggdata) %in% drops)]
aggdata   = merge(aggdata, activity_labels)

write.csv(file="submit.csv", x=aggdata)