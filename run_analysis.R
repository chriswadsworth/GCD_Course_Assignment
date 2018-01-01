#Download and decompress zip file
run_analysis <- function() {
        library(dplyr)
        
        # 0: Check to see if the dataset is in the working directory.  If not, download and unzip into wd.
        if (!file.exists("./dataset.zip")) {
                download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
                              dest = "dataset.zip", mode = "wb")
                unzip("dataset.zip")
        }
        
        # 1: Read features for variable names (and clean them), and find features related to mean or std
        features <- read.table("./UCI HAR Dataset/features.txt")
                features$V2 <- gsub("[_-]",".",features$V2)
                features$V2 <- gsub("[()]", "", features$V2)
        mean_related <- grep("[Mm]ean", features$V2)
        std_related <- grep("std", features$V2)
        mean_std_related <- sort(union(mean_related, std_related))
        
        # 2: Set variable names for x_test from features, and retain mean, std variables. Repeat for training set.
        x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
        colnames(x_test) <- features$V2
        x_test <- x_test[,mean_std_related]
        
        x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
        colnames(x_train) <- features$V2
        x_train <- x_train[,mean_std_related]
        
        # 3: Combine test subject file, test label file, and x_test.  Repeat for training set.
        test_subj <- read.table("./UCI HAR Dataset/test/subject_test.txt")
        colnames(test_subj) <- "subject"
        test_act <- read.table("./UCI HAR Dataset/test/y_test.txt")
        colnames(test_act) <- "activity"
        test_all <- cbind(test_subj, test_act, x_test)
        
        train_subj <- read.table("./UCI HAR Dataset/train/subject_train.txt")
        colnames(train_subj) <- "subject"
        train_act <- read.table("./UCI HAR Dataset/train/y_train.txt")
        colnames(train_act) <- "activity"
        train_all <- cbind(train_subj, train_act, x_train)
        
        # 4: Combine test_all and train_all into all_results, and replace activity code with description.
        all_results <- rbind(train_all, test_all)
        act_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
        for (i in 1:6){
                all_results$activity <- sub(i, act_labels[i,2], all_results$activity)
        }
        
        # 5: Create dataframe of averages of each subject-activity pair and compute averages
        subjects <- sort(unique(all_results$subject))
        nsub <- length(subjects)
        activities <- sort(unique(all_results$activity))
        nact <- length(activities)
        ave_results <- as.data.frame(matrix(nrow=nsub*nact,ncol=ncol(all_results))) 
                colnames(ave_results) <- colnames(all_results) 
                ave_results$subject = rep(subjects,each=6)
                ave_results$activity = rep(activities)
        for (i in 1:nrow(ave_results)) {
                temp <- all_results[all_results$subject == ave_results[i,1]
                                    & all_results$activity == ave_results[i,2],]
                ave_results[i,3:ncol(ave_results)] <- lapply(temp[,3:ncol(temp)], mean)
        }
        
        # 6: Return results to the global environment and write average results
        all_results <<- all_results
        ave_results <<- ave_results
        return(head(ave_results[,1:5],12))
        write.table(ave_results, "ave_results.txt", row.names = FALSE)
}