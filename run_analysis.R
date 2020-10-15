## install.packages("dplyr")
library(dplyr)
## Downloading the data and preparing the data
## Use the directory ".data' as working directory
currentdir <- "./data"
if(!dir.exists("./data")) dir.create("./data")
setwd(currentdir)

downloadedurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipfile <- "UCI HAR Dataset.zip"
download.file(downloadurl, zipfile)

if(file.exists(zipfile)) unzip(zipfile)

##If all files present proceed to create the file descriptors
basedir <- "UCI HAR Dataset"
featuresfile <- paste(basedir, "features.txt", sep="/")
activitylabelsfile <- paste(basedir, "activity_labels.txt", sep="/")
testvariablesfile <- paste(basedir, "test/X_test.txt", sep="/")
testactivityfile <- paste(basedir, "test/y_test.txt", sep="/")
testsubjectfile <- paste(basedir, "test/subject_test.txt", sep="/")
trainvariablesfile <- paste(basedir, "train/X_train.txt", sep="/")
trainactivityfile <- paste(basedir, "train/y_train.txt", sep="/")
trainsubjectfile <- paste(basedir, "train/subject_train.txt", sep="/")
neededfiles <- c(featuresfile,
                 activitylabelsfile,
                 testvariablesfile,
                 testactivityfile,
                 testsubjectfile,
                 trainvariablesfile,
                 trainactivityfile,
                 trainsubjectfile
)
sapply(neededfiles, function(f) if(!file.exists(f)) stop(paste("Needed file ", f, " doesn't exist. Exitting ...", sep="")))

## Read featuresfile
features <- read.table(featuresfile, col.names=c("rownumber","variablename"))

## Cleaning all the variables from features
allvariables <- 
  mutate(features, variablename = gsub("BodyBody", "Body", variablename))

## Tidying the variable name to remove special characters and lowercase all characters
allvariables <- mutate(allvariables, variablename = gsub("-", "", variablename),
                       variablename = gsub("\\(", "", variablename),
                       variablename = gsub("\\)", "", variablename),
                       variablename = tolower(variablename))

## Filter the needed variables - mean() and std()
neededvariables <- filter(allvariables, grepl("mean\\(\\)|std\\(\\)", variablename))

## Make the neededvariables readable
##    Remove special characters, Convert to lower case
neededvariables <- mutate(neededvariables, variablename = gsub("-", "", variablename),
                          variablename = gsub("\\(", "", variablename),
                          variablename = gsub("\\)", "", variablename),
                          variablename = tolower(variablename))

## Read activitylabelsfile
activitylabels <- read.table(activitylabelsfile, col.names=c("activity", "activitydescription"))
## Read in test data stats

testvalues <- read.table(testvariablesfile, col.names = allvariables$variablename)
testneededvalues <- testvalues[ , neededvariables$variablename]

## Read in test activities
testactivities <- read.table(testactivityfile, col.names=c("activity"))

## Read in test subjects
testsubjects <- read.table(testsubjectfile, col.names=c("subject"))

## Add a readable activity description
testactivitieswithdescr <- merge(testactivities, activitylabels)

## Put the test data together
##    Assuming that the data is in the same order and all we need is cbind
##    Combining values, activities, subjects

testdata <- cbind(testactivitieswithdescr, testsubjects, testneededvalues)
## Read in train variables

trainvalues <- read.table(trainvariablesfile, col.names = allvariables$variablename)
trainneededvalues <- trainvalues[ , neededvariables$variablename]

## Read in train activities
trainactivities <- read.table(trainactivityfile, col.names=c("activity"))

## Add a readable activity description
trainactivitieswithdescr <- merge(trainactivities, activitylabels)

## Put the train data together
##    Assuming that the data is in the same order and all we need is cbind
##    Combining values, activities, subjects
traindata <- cbind(trainactivitieswithdescr, trainsubjects, trainneededvalues)

## Combine the testdata and traindata
## Additionally make subject a factor
alldata <- rbind(testdata, traindata) %>% select( -activity )
alldata <- mutate(alldata, subject = as.factor(alldata$subject))

## Write the data out
write.table(alldata, "Mean_And_StdDev_For_Activity_Subject.txt")

## Create a second, independent tidy data set with the average of each 
##        variable for each activity and each subject.
## Group the data by activity, subject
allgroupeddata <- group_by(alldata,activitydescription,subject)

## Get the average of each variable
summariseddata <- summarise_each(allgroupeddata, funs(mean))

## Write the data out
write.table(summariseddata, "Average_Variable_By_Activity_Subject.txt", row.names = FALSE)