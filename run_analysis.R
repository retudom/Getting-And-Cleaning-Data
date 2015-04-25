# Download and unzip the data set from the given source and extract it locally
DownloadDataSet = function(url) {
  if (!file.exists("data")) {
    #  Check for existing data directory or create it
    message("create data folder...")
    dir.create("data")
  }
  if (!file.exists("data/UCI HAR Dataset")) {
    # if data set isn't existing download it
    #sourceURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    zipfile="data/UCI_HAR_data.zip"
    message("data download...")
    download.file(url, destfile=zipfile, method="auto")
    unzip(zipfile, exdir="data")
  }
}

## load the train and test files 
LoadMergeData = function() {
  message("load data...")
  ## build the common path to the data files
  path<<-paste(getwd(),"/data/UCI HAR Dataset/", sep = "")
  
  ## read X_train.txt into DF "train.dat"
  message("  read X_train.txt...")
  train.dat = read.csv(paste(path,"train/X_train.txt",sep=""), sep="", header=FALSE)
  
  ## append Y_train.txt to DF "train.dat" as variable 562
  message("  read Y_train.txt...")
  train.dat[,ncol(train.dat)+1] = read.csv(paste(path,"train/Y_train.txt",sep=""), sep="", header=FALSE)
  
  ## append subject_train.txt to DF "train.dat" as variable 563
  message("  read subject_train.txt...")
  train.dat[,ncol(train.dat)+1] = read.csv(paste(path,"train/subject_train.txt",sep=""), sep="", header=FALSE)
  
  ## read X_test.txt into DF "test.dat"
  message("  read X_test.txt...")
  test.dat = read.csv(paste(path,"test/X_test.txt",sep=""), sep="", header=FALSE)
  
  ## append Y_test.txt to DF "test.dat" as variable 562
  message("  read Y_test.txt...")
  test.dat[,ncol(test.dat)+1] = read.csv(paste(path,"test/Y_test.txt",sep=""), sep="", header=FALSE)
  
  ## append subject_test.txt to DF "test.dat" as variable 563
  message("  read subject_test.txt...")
  test.dat[,ncol(test.dat)+1] = read.csv(paste(path,"test/subject_test.txt",sep=""), sep="", header=FALSE)
  
  ##############################################################
  ## 1. Merge train.dat and test.dat together into DF Data
  ## Merges the training and the test sets to create one data set.
  ##################################################################
  message("merge data...")
  rbind(train.dat, test.dat)
}

#############################################################################################
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
#############################################################################################
ExtractData=function(df){
  
  message("extract data...")
  ## read the features
  features <- read.csv(paste(path,"features.txt", sep=""), sep="", header=FALSE)
  
  ## find the relevant columns having "-mean"or "-std" in column name
  cols.in.scope <<- grep(".*-mean.*|.*-std.*", features[,2])
  
  
  ## set the features in scope (global)
  features <<- features[cols.in.scope,]
  
  var.count = ncol(df)
  ## also add the two columns for Subject and Activity (the last two columns of Data)
  cols.in.scope <<- c(cols.in.scope, var.count-1, var.count)
  
  ## remove the obsolete columns from DF Data
  ## Data <- Data[,cols.in.scope]
  df<-df[,cols.in.scope]
  df
}

############################################################################
## 3. Uses descriptive activity names to name the activities in the data set
############################################################################
SetActivityNames = function(df){
  message("set activity labels...")
  ## read the activity lables into DF activity.Labels)
  activity.Labels = read.csv(paste(path,"activity_labels.txt", sep=""), sep="", header=FALSE)
  
  ## set the matching activity label for each row
  activity.ID = 1
  for (ActivityLabel in activity.Labels$V2) {
    df$activity <- gsub(activity.ID, ActivityLabel, df$activity)
    activity.ID <- activity.ID + 1
  }
  
  df
}

#######################################################################
## 4. Appropriately labels the data set with descriptive variable names
#######################################################################
DescriptiveVariables = function(df){
  message("make descriptive variable names...")
  ## make suitable feature names for R using substitutions 
  features[,2] <- gsub('-meanFreq()', '.mean.freq', features[,2]) # substitutes "-meanFreq()" with ".mean.freq"
  features[,2] <- gsub('-mean()', '.mean', features[,2]) # substitutes "-mean" with ".mean"
  features[,2] <- gsub('-std()', '.std', features[,2]) # substitutes "-std" with ".std"
  features[,2] <- gsub('[-]', '.', features[,2]) # substitutes "-" with "."
  features[,2] <- gsub('[()]', '', features[,2]) # removes "()"
  
  ## set the column names (as of DF features 2nd column) for DF Data
  colnames(df) <- c(features$V2, "Activity", "Subject")
  ## make all names lowercase
  colnames(df) <- tolower(colnames(df))
  
  df
}

##################################################################################################################
## 5. Creates a second, independent DF tidy.data with the mean of each variable for each activity and each subject.
##################################################################################################################
MakeTidy = function(df){
  message("tidy data...")
  ## declare Activity and Subject as nominal data
  df$activity <- as.factor(df$activity)
  df$subject <- as.factor(df$subject)
  
  ## aggregate DF Data by Activity and Subject while calculating the mean function
  #5.2.1 define the number of colums in DF Data minus the nominal columns (activity and subject)
  countnndc = ncol(df)-2 # the count of colums with non nominal data
  nndc = c(1:countnndc) # the colums with non nominal data
  
  ## aggregate and calculate the mean only for the columns containing measured values
  ## tidy.Data = aggregate(Data[,c(1:nndc)], by=list(Activity = Data$Activity, Subject=Data$Subject), mean, na.rm=TRUE)
  tidy <- aggregate(df[,nndc], by=list(activity = df$activity, subject=df$subject), mean, na.rm=TRUE)
  tidy
}

## Preparation:
DownloadDataSet("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip")

## 1. Merges the training and the test sets to create one data set.
Data <- LoadMergeData()

## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
Data <- ExtractData(Data)

## 4. Appropriately labels the data set with descriptive variable names. 
Data <- DescriptiveVariables(Data)

## 3. Uses descriptive activity names to name the activities in the data set
Data <- SetActivityNames(Data) # reasonable to be run after labeling because of script design

## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
Tidy.Data <- MakeTidy(Data)

## Completion
message("write tidy.txt...")
write.table(Tidy.Data, "tidy.txt", sep="\t",row.names = F)
message("Done!")
message("Find the tidy data in file: \"",paste(getwd(),"/tidy.txt\"",sep=""))

## for CodeBook
write(names(Data), file = "variables.txt", ncolumns = 1)
