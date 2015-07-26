
run_analysis <- function(){
  
  setwd("./data/Dataset/UCI HAR Dataset")


  # Read subjects measurements in the train set
  subject_train <- read.table("./train/subject_train.txt")
  colnames(subject_train) <- c("subject")
  # Read values recorded in the train set
  values_train <- read.table("./train/X_train.txt")
  # Read the activities in the train set
  activity_train <- read.table("./train/y_train.txt")
  colnames(activity_train) <- c("activity")
  
  #Read features list
  features_list <- read.table("./features.txt")
  features <- as.character(features_list$V2)
  # Remove unwanted characters from the colnames
  features <- gsub("\\(", "", features)
  features <- gsub("\\)", "", features)
  features <- gsub("-", "", features)
  features <- gsub(",","",features)
  features <- gsub("mean", "Mean", features)
  features <- gsub("std", "Std", features)
  
  # data frame for the training set
  train_data <- data.frame(subject_train$subject, activity_train$activity, values_train)
  colnames(train_data) <- c("Subject","Activity", features)
  
  # Read subjects measurements in the test set
  subject_test <- read.table("./test/subject_test.txt")
  colnames(subject_test) <- c("subject")
  # Read values recorded in the test set
  values_test <- read.table("./test/x_test.txt")
  # Read the activities in the test set
  activity_test <- read.table("./test/y_test.txt")
  colnames(activity_test) <- c("activity")
  
  # data frame for the test set
  test_data <- data.frame(subject_test$subject, activity_test$activity, values_test)
  colnames(test_data) <- c("Subject","Activity", features)
  
  # Merge the training and test sets to create one data set 
  merged_data <- rbind(train_data,test_data)
  #merged_data <- merge(train_data, test_data, all = TRUE)
  #mean and standard deviation for each measurement. 
  mean_indices <- grep("Mean",features)
  std_indices <- grep("Std",features)
  indices <- c(mean_indices,std_indices)
  indices <- sort(indices)
  indices <- indices + 2
  
  data <- data.frame(merged_data$Subject, merged_data$Activity, merged_data[,indices])
  colnames(data)[1] <- "Subject"
  colnames(data)[2] <- "Activity"
  
  # Descriptive variable names with appropriate labels
  for(i in 1:length(data$Activity)){
    if(data$Activity[i] == 1)
      data$Activity[i] <- "WALKING"
    else if(data$Activity[i] == 2)
      data$Activity[i] <- "WALKING_UPSTAIRS"
    else if(data$Activity[i] == 3)
      data$Activity[i] <- "WALKING_DOWNSTAIRS"
    else if(data$Activity[i] == 4)
      data$Activity[i] <- "SITTING"
    else if(data$Activity[i] == 5)
      data$Activity[i] <- "STANDING"
    else if(data$Activity[i] == 6)
      data$Activity[i] <- "LAYING"    
  }
  
  # Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  
  tidy_data <- aggregate(data, by=list(data$Subject, data$Activity), FUN=mean, na.rm=TRUE)
  tidy_data <- subset(tidy_data, select = -c(Subject,Activity) )
  colnames(tidy_data)[1] <- "Subject"
  colnames(tidy_data)[2] <- "Activity"
  
  tidy_data 
  
  write.table(tidy_data, "tidy_data.txt", row.names=FALSE)
}