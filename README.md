# Markdown file for run_analysis.R
created by: Chris Wadsworth
date: 1/1/2018

## Create the combined dataset. 

* The first major function requires seven different files: features, X_test, y_test, subject_test, X_train, y_train, subject_train, and activity_labels.

* Before beginning, need to make sure that the files are accessable in the working directory.  The following code checks to see if the file is downloaded.  If not, then the zip file is downloaded from the url and unpacked into the working directory.

```{r File Download}
if (!file.exists("./dataset.zip")) {
        download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
              dest = "dataset.zip", mode = "wb")
        unzip("dataset.zip")
}
```

### Step 1:  Read, clean, and index the desired variable names

* Next, read the features table, clean the variable names, and find the variables that correspond to either mean or standard deviation (std).

* Use gsub to remove parentheses and replace underscores and dashes with periods.  Note, that the values in x_train and x_test later are normalized; thus, there are no units.

* Use grep to return indices of variable names related to either mean or std.

* Finally, return indices of both mean- or std-related variables as mean_std_related.

```{r}
features <- read.table("./UCI HAR Dataset/features.txt")
        features$V2 <- gsub("[_-]",".",features$V2)
        features$V2 <- gsub("[()]", "", features$V2)
mean_related <- grep("[Mm]ean", features$V2)
std_related <- grep("std", features$V2)
mean_std_related <- sort(union(mean_related, std_related))
```

### Step 2:  Read the test and train results

* Read the test results (X_test), set the variable names from the features table, and return only those columns that are related to mean or std.

* Repeat for the train results.

```{r}
x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
colnames(x_test) <- features$V2
x_test <- x_test[,mean_std_related]
        
x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
colnames(x_train) <- features$V2
x_train <- x_train[,mean_std_related]
```

### Step 3:  Create combined datasetsfor test and train datasets

* Read subject_test and y_test tables and label variables acccordingly.

* Combine all test set data into test_all using cbind.

* Repeat for training set.

```{r}
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
```

### Step 4:  Create combined dataset from training and testing sets

* Use rbind to create all_results dataframe

* Read activity labels from activity_labels.txt

* Replace activity identifiers with descriptions from activity_labels.txt

```{r}
all_results <- rbind(train_all, test_all)
act_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
for (i in 1:6){
        all_results$activity <- sub(i, act_labels[i,2], all_results$activity)
}
```

### Step 5:  Create separate dataset with averages for each variable by subject and activity

* Identify unique subjects and number of subjects (nsub)

* Identify unique activities and number of unique activities (nact)

* Create empty dataframe (ave_results) with nsub x nact number of rows and same number of columns as all_results

* Set column names of ave_results to be the same as all_results before appending "avg." to the beginning of the variable names after activity.

* Fill subject variable with six repetitions of each subject

* Fill activity variable with repetitions of each activity

* End result is all subject-activity pairs being represented once in ave_results

* The for loop fills in the remainder of the ave_results dataframe.

* A temporary variable ("temp") that is the subset of all_results for the subject and activity of the row i in ave_results.

* The unfilled variables in the ith row of ave_results are filled with the mean of the corresponding columns in temp using lapply.

```{r}
subjects <- sort(unique(all_results$subject))
nsub <- length(subjects)
activities <- sort(unique(all_results$activity))
nact <- length(activities)
ave_results <- as.data.frame(matrix(nrow=nsub*nact,ncol=ncol(all_results))) 
colnames(ave_results) <- colnames(all_results) 
colnames(ave_results)[3:ncol(ave_results)] <- paste0("avg.",colnames(ave_results)[3:ncol(ave_results)])
ave_results$subject = rep(subjects,each=6)
ave_results$activity = rep(activities)
for (i in 1:nrow(ave_results)) {
        temp <- all_results[all_results$subject == ave_results[i,1]
                            & all_results$activity == ave_results[i,2],]
        ave_results[i,3:ncol(ave_results)] <- lapply(temp[,3:ncol(temp)], mean)
}
```

## EXPORT RESULTS

* Export all_results and ave_results to global environment

* Write ave_results to a .txt file in the working directory

* Return first 5 columns, 12 rows of ave_results

```{r}
all_results <<- all_results
ave_results <<- ave_results
write.table(ave_results, "ave_results.txt", row.names = FALSE)
return(head(ave_results[,1:5],12))
```
