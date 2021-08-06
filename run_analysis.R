UNZIP_FOLDER_PATH <- '/Users/ciguaran/Downloads/UCI HAR Dataset/'
open_resource <- function(subfolder =NULL, filename, header=FALSE, sep="") {
  if (is.null(subfolder)) {
    return(read.csv(paste(paste(c(UNZIP_FOLDER_PATH, filename), collapse = "")), header = header, sep = sep))
  }else{
    return(read.csv(paste(paste(c(UNZIP_FOLDER_PATH, subfolder, filename), collapse = "")), header = header, sep = sep))
  }
}

y_train <- open_resource('train/', 'y_train.txt', header=FALSE)
# For x_train using table since it is smarter realizing double spaces.
x_train <- read.table('/Users/ciguaran/Downloads/UCI HAR Dataset/train/X_train.txt')
subject_train <- open_resource('train/', 'subject_train.txt', header = FALSE)
subject_test <- open_resource('test/', 'subject_test.txt', header=FALSE)
x_test <- read.table('/Users/ciguaran/Downloads/UCI HAR Dataset/test/X_test.txt')
y_test <- open_resource('test/', 'y_test.txt', header = FALSE)
activity_labels <- open_resource(NULL, 'activity_labels.txt', sep= " ")
activity_labels <- rename(activity_labels, activityLabel = V2, activity_id = V1, )
subject_train <- rename(subject_train, subject = V1)
subject_test <- rename(subject_test, subject = V1)
features <- open_resource(NULL, 'features.txt', sep=" ")

# Lets add subjet information
x_train <- cbind(x_train, subject_train)
x_test <- cbind(x_test, subject_test)

#Item1  Merges the training and the test sets to create one data set.
x_merged <- rbind(x_train, x_test)
y_merged <- rbind(y_train, y_test)

#Item 4: Appropriately labels the data set with descriptive variable names. 
names(x_merged) <- c(features$V2, 'subject')

# Item 3: Uses descriptive activity names to name the activities in the data set
y_merged <- merge(y_merged, activity_labels, by.x = 'V1', by.y='activity_id')
y_merged <- y_merged['activityLabel']

full_dataset <- cbind(x_merged, y_merged)

head(full_dataset)

# Item 2 Extracts only the measurements on the mean and standard deviation for each measurement. 
columns <- names(full_dataset)
mean_columns <- columns[str_detect(columns, "mean") & ! str_detect(columns, "meanFreq")]
std_columns <- columns[str_detect(columns, "std")]

full_dataset <- full_dataset[,c(mean_columns, std_columns, 'activityLabel', 'subject')]
write.csv(df,"./full_dataset_week4.csv", row.names = FALSE)

#Item 5creates a second, independent tidy data set with the average of each variable for each activity and each subject.
library(tidyverse)
summarized_dataset <- full_dataset %>% 
  group_by(subject,activityLabel, .add=TRUE) %>%
  summarise(across(everything(), list(mean = mean)))


write.table(summarized_dataset, "./summarized_dataset.csv", row.names = FALSE)
