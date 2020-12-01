run_analysis <- function(){
  X_train_path <- 'UCI HAR Dataset/train/X_train.txt'
  X_train <- read.table(X_train_path)
  y_train_path <- 'UCI HAR Dataset/train/y_train.txt'
  y_train <- read.table(y_train_path)
  subject_train_path<- 'UCI HAR Dataset/train/subject_train.txt'
  subject_train <- read.table(subject_train_path)
  
  X_test_path <- 'UCI HAR Dataset/test/X_test.txt'
  X_test <- read.table(X_test_path)
  y_test_path <- 'UCI HAR Dataset/test/y_test.txt'
  y_test <- read.table(y_test_path)
  subject_test_path<- 'UCI HAR Dataset/test/subject_test.txt'
  subject_test <- read.table(subject_test_path)
  
  feature_path <- 'UCI HAR Dataset/features.txt'
  features <- read.table(feature_path)
  
  train <- cbind(subject_train, X_train, y_train)
  #names(train) <- c('subject number', features[,2], 'y_label')
  
  test <- cbind(subject_test, X_test, y_test)
  #names(test) <- c('subject_number', features[,2], 'y_label')
  
  
  final <- rbind(train, test)
  names(final) <- c('subject_number', features[,2], 'y_label')
  
  activity_path <- 'UCI HAR Dataset/activity_labels.txt'
  activity_label <- read.table(activity_path)
  final$y_label <- sapply(final$y_label, function(elem) 
    activity_label[activity_label[,1] == elem,2])
  
  mean_std <- final[,grep('mean|std|y_label|subject_number', names(final))]
  mean_std
  
  library(dplyr)
  table_df <- tbl_df(mean_std)
  group <- group_by(table_df, y_label, subject_number)
  summ <-summarise_at(group, 2:79, mean)
  summ
}