run_analysis <- function(){
    
    # setwd("./R/Data Cleaning/Week 4/UCI HAR Dataset")
    library(dplyr)
    
    # enrich test and train data with subject and activity seperately
    test_data <- bind_cols(read.table("./test/subject_test.txt"), 
                           read.table("./test/Y_test.txt"), 
                           read.table("./test/X_test.txt"))
    
    train_data <- bind_cols(read.table("./train/subject_train.txt"), 
                            read.table("./train/Y_train.txt"), 
                            read.table("./train/X_train.txt"))
    
    # Apply descriptive column names as a preparation for the merge
    features <- scan("features.txt", what = "character", sep = "/")
    colnames(test_data) <- c("Subject", "Activity", features)
    colnames(train_data) <- c("Subject", "Activity", features)
    
    # Do the merge of train and test data
    data <- bind_rows(test_data, train_data)
    
    # Filter on columns containing "mean()" and "std()"
    data <- data[,c(1,2,grep("std\\(\\)|mean\\(\\)", colnames(data)))]
    
    # Use descriptive activity names to name the activities in the data set
    labels <- read.table("activity_labels.txt", colClasses = "character")
    data$Activity <- plyr::mapvalues(data$Activity, labels[,1], labels[,2])
    
    # Average all variables for each activity (col 2) and subject (col 1)
    data <- aggregate(data[,3:ncol(data)], by = list(data[,1], data[,2]), FUN = mean)
    names(data)[1] <- "Subject"
    names(data)[2] <- "Activity"
    
    # Create tidy data set
    write.table(data, "SamsungTidyMean.txt", row.names = FALSE)
    
    }