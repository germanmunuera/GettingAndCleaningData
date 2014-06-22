load_and_bind_data <- function() {
    if(!file.exists("./UCI HAR Dataset/test/subject_test.txt")) {
        stop("test/subject_test.txt not found")
    }
    if(!file.exists("./UCI HAR Dataset/test/X_test.txt")) {
        stop("test/X_test.txt not found")
    }
    if(!file.exists("./UCI HAR Dataset/test/y_test.txt")) {
        stop("test/y_test.txt not found")
    }
    
    # Read Subjects, Signals and Activity labels from test
    testSubjects <- read.table("./UCI HAR Dataset/test/subject_test.txt", col.names=c("subject"))
    testSignals <- read.table("./UCI HAR Dataset/test/X_test.txt")
    testActivityLabels <- read.table("./UCI HAR Dataset/test/y_test.txt", col.names=c("activity"))
    
    # Create test data frame binding columns (subject, V1, V2, ... V561, activity)
    test <- cbind(testSubjects)
    test <- cbind(test, testSignals)
    test <- cbind(test, testActivityLabels)
    
    if(!file.exists("./UCI HAR Dataset/train/subject_train.txt")) {
        stop("test/subject_train.txt not found")
    }
    if(!file.exists("./UCI HAR Dataset/train/X_train.txt")) {
        stop("test/X_train.txt not found")
    }
    if(!file.exists("./UCI HAR Dataset/train/y_train.txt")) {
        stop("test/y_train.txt not found")
    }
    
    # Read Subjects, Signals and Activity labels from train
    trainSubjects <- read.table("./UCI HAR Dataset/train/subject_train.txt", col.names=c("subject"))
    trainSignals <- read.table("./UCI HAR Dataset/train/X_train.txt")
    trainActivityLabels <- read.table("./UCI HAR Dataset/train/y_train.txt", col.names=c("activity"))
    
    # Create train data frame binding columns (V1, V2, ... V561, subject, activity)
    train <- cbind(trainSignals)
    train <- cbind(train, trainSubjects)
    train <- cbind(train, trainActivityLabels)
    
    # Join test and train data frames
    data <- rbind(test, train)
    
    return (data)
}

extract_and_rename_cols <- function(data){
    data <- data[,c(1,2,3,4,5,6,7,42,43,44,45,46,47,82,83,84,85,86,87,122,123,124,125,126,127,162,163,
                    164,165,166,167,202,203,215,216,228,229,241,242,254,255,267,268,269,270,271,272,346,
                    347,348,349,350,351,425,426,427,428,429,430,504,505,517,518,530,531,543,544,563)]
    
    colnames(data) <- c("subject","tBodyAcc_mean_X","tBodyAcc_mean_Y","tBodyAcc_mean_Z","tBodyAcc_std_X",
                        "tBodyAcc_std_Y","tBodyAcc_std_Z","tGravityAcc_mean_X","tGravityAcc_mean_Y",
                        "tGravityAcc_mean_Z","tGravityAcc_std_X","tGravityAcc_std_Y","tGravityAcc_std_Z",
                        "tBodyAccJerk_mean_X","tBodyAccJerk_mean_Y","tBodyAccJerk_mean_Z",
                        "tBodyAccJerk_std_X","tBodyAccJerk_std_Y","tBodyAccJerk_std_Z","tBodyGyro_mean_X",
                        "tBodyGyro_mean_Y","tBodyGyro_mean_Z","tBodyGyro_std_X","tBodyGyro_std_Y",
                        "tBodyGyro_std_Z","tBodyGyroJerk_mean_X","tBodyGyroJerk_mean_Y",
                        "tBodyGyroJerk_mean_Z","tBodyGyroJerk_std_X","tBodyGyroJerk_std_Y",
                        "tBodyGyroJerk_std_Z","tBodyAccMag_mean","tBodyAccMag_std","tGravityAccMag_mean",
                        "tGravityAccMag_std","tBodyAccJerkMag_mean","tBodyAccJerkMag_std",
                        "tBodyGyroMag_mean","tBodyGyroMag_std","tBodyGyroJerkMag_mean",
                        "tBodyGyroJerkMag_std","fBodyAcc_mean_X","fBodyAcc_mean_Y","fBodyAcc_mean_Z",
                        "fBodyAcc_std_X","fBodyAcc_std_Y","fBodyAcc_std_Z","fBodyAccJerk_mean_X",
                        "fBodyAccJerk_mean_Y","fBodyAccJerk_mean_Z","fBodyAccJerk_std_X",
                        "fBodyAccJerk_std_Y","fBodyAccJerk_std_Z","fBodyGyro_mean_X","fBodyGyro_mean_Y",
                        "fBodyGyro_mean_Z","fBodyGyro_std_X","fBodyGyro_std_Y","fBodyGyro_std_Z",
                        "fBodyAccMag_mean","fBodyAccMag_std","fBodyBodyAccJerkMag_mean",
                        "fBodyBodyAccJerkMag_std","fBodyBodyGyroMag_mean","fBodyBodyGyroMag_std",
                        "fBodyBodyGyroJerkMag_mean","fBodyBodyGyroJerkMag_std","activity")
    return (data)
}

merge_activity_labels <- function(data) {
    if(!file.exists("./UCI HAR Dataset/activity_labels.txt")) {
        stop("activity_labesl.txt not found")
    }
    
    # Read Activity names
    activityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt", col.names=c("activity_id", "activity_name"))
    
    # Join data with activity names
    data <- merge(data, activityLabels, by.x="activity", by.y="activity_id", all.x=TRUE)
    data$activity <- NULL
    
    return (data)
}

run_analysis <- function() {
    # Load files and bind columns and rows
    data <- load_and_bind_data()
    
    # Extract mean & avg columns and rename all columns
    data <- extract_and_rename_cols(data)
    
    # Merge data with activity labels (join activity ids)
    data <- merge_activity_labels(data)
    
    # Group by activity name & subject and calculate the mean for each variable
    tidy <- aggregate( . ~ activity_name + subject, data, mean )
    
    # Write results to txt file
    write.table(tidy, file="tidy.txt", row.names=FALSE, col.names=TRUE)
    
    return (tidy)
}

tidy <- run_analysis()