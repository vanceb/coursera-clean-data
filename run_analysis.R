############################################################################################
############################################################################################
# Set up some key variables
############################################################################################
############################################################################################

setwd('/Users/vance/Documents/Coursera/Cleaning Data/Course Project')
rawDataDir <- "./data/raw/UCI HAR Dataset/"

dataURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
dataName <- "dataset.zip"


############################################################################################
############################################################################################
# Helper functions
############################################################################################
############################################################################################

############################################################################################
# make_data_dirs()
# Makes the data directories if they don't exist
############################################################################################
make_data_dirs <- function() {
  # Make the data directories if they don't exist
  if (!file.exists('./data')) {
    dir.create('./data')
  }
  if (!file.exists('./data/raw')) {
    dir.create('./data/raw')
  }
  if (!file.exists('./data/processing')) {
    dir.create('./data/processing')
  }
  if (!file.exists('./data/tidy')) {
    dir.create('./data/tidy')
  }
}

############################################################################################
# download()
# Downloads and unzips the data files
############################################################################################
download <- function (fileURL, fileName) {
  # Make sure we have the necessary directories
  make_data_dirs()
  
  # Change the WD for file download
  setwd('./data/raw')
  
  # Download the files  
  download.file(fileURL, fileName, method="curl")
  
  # Make a note of the date we downloaded the data
  fileDownloaded <- paste0(fileName, ".download.date")
  dateDownloaded <- date()
  write(dateDownloaded, fileDownloaded, append = FALSE)
  
  # Unzip the data file
  unzip(dataName, overwrite=TRUE)
  
  # Set the working directory back
  setwd("../..")
}

############################################################################################
# merge_load()
# Merges the "Test" and "Train" datasets
############################################################################################
merge_load <- function(baseDir, testFile, trainFile) {
  library(data.table)
  test <- fread(paste0(baseDir, testFile))
  train <- fread(paste0(baseDir, trainFile))
  rbind(test, train)
}

############################################################################################
# merge_load_fw()
# Merges the "Test" and "Train" datasets, but for fixed width files
############################################################################################
merge_load_fw <- function(baseDir, testFile, trainFile, fieldwidth) {
  library(data.table)
  test <- read.fwf(paste0(baseDir, testFile), fieldwidth)
  train <- read.fwf(paste0(baseDir, trainFile), fieldwidth)
  data.table(rbind(test, train))
}


############################################################################################
############################################################################################
# Process the Data - This is the master function wich calls all helper functions
############################################################################################
############################################################################################

process_data <- function() {
  ############################################################################################
  # 1. Merges the training and the test sets to create one data set.
  ############################################################################################
  
  # Download and unzip the dataset if it doesn't exist
  if (!file.exists(paste0('./data/raw/', dataName))) {
    download(dataURL, dataName)
  }
  
  # Merge the test and train data sets - these will be put into data frames
  subject <- merge_load(rawDataDir, "test/subject_test.txt", "train/subject_train.txt")
  X <- merge_load_fw(rawDataDir, "test/X_test.txt", "train/X_train.txt", rep(16, 561))
  y <- merge_load(rawDataDir, "test/y_test.txt", "train/y_train.txt")
  
  # Load the field names
  featureNames <- read.table(paste0(rawDataDir, "features.txt"), stringsAsFactors = FALSE)[[2]]
  # Apply the names to the data.table
  #names(X) <- featureNames
  
  
  ############################################################################################
  # 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
  ############################################################################################

  # Convert to a dplyr dataset
  library(dplyr)
  X_df <- tbl_df(X)
  
  # Pull out the Mean and SD columns
  msd_cols <- grep("std()|mean()", featureNames)
  msd_df <- select(X_df, msd_cols)
  
  # Apply the column names to the table - Note: partially meets requirements of #4 below
  names(msd_df) <- featureNames[msd_cols]
  
  
  ############################################################################################
  # 3. Uses descriptive activity names to name the activities in the data set
  ############################################################################################
  
  # Load the Activity Labels
  act_labels <- tbl_df(read.table("./data/raw/UCI HAR Dataset/activity_labels.txt", sep=" "))
  
  # Create a vector of useful names from the y vector
  y <- tbl_df(y) # Need to convert to dplyr format to allow the join
  activity <- inner_join(y, act_names)
  names(activity) <- c("Activity_ID", "Activity")
  
  # Add the subjects and Activities to the Dataset
  msd_df <- mutate(msd_df, subject=subject$V1, activity=activity$Activity)

  
  ############################################################################################
  # 4. Appropriately labels the data set with descriptive variable names. 
  ############################################################################################
  
  # This has been done as we have been generating the data

  
  ############################################################################################
  # 5. From the data set in step 4, creates a second, independent tidy data set with the 
  # average of each variable for each activity and each subject.
  ############################################################################################
  
  # Use tidyr to transform the dataset
  library(tidyr)
  
  tidy_df <- msd_df %>%
    gather(variable, value, 1:79)
  
  # Set up groups
  groups <- group_by(tidy_df, subject, activity, variable)
  
  # Create the summary dataset
  summary <- summarise(groups, mean(value))
  
  # Write out the Tidy Dataset
  write.table(summary, file="./data/tidy/summary.txt", row.name=FALSE)
  
}

# Run the process_data() function to generate the tidy dataset in the ./data/tidy directory
process_data()