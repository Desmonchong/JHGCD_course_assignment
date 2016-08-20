##################### save original working directory ################################
Ori_WD <- getwd()
######################################################################################

############################### test package #########################################
list.of.packages <- c("dplyr","data.table")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
######################################################################################

######################### Download file and unzip ####################################
filename <- "dataset.zip"

if (!file.exists(filename)){
        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(fileURL, filename, method="libcurl")
}  
if (!file.exists("UCI HAR Dataset")) { 
        unzip(filename) 
}
######################################################################################

#########1) Merges the training and the test sets to create one data set.#############

filesPath <- file.path(Ori_WD,"UCI HAR Dataset")
# load subject files
SubjectTrain <- tbl_df(read.table(file.path(filesPath, "train", "subject_train.txt")))
SubjectTest  <- tbl_df(read.table(file.path(filesPath, "test" , "subject_test.txt" )))

# load activity files
ActivityTrain <- tbl_df(read.table(file.path(filesPath, "train", "Y_train.txt")))
ActivityTest  <- tbl_df(read.table(file.path(filesPath, "test" , "Y_test.txt" )))

#load data files.
DataTrain <- tbl_df(read.table(file.path(filesPath, "train", "X_train.txt" )))
DataTest  <- tbl_df(read.table(file.path(filesPath, "test" , "X_test.txt" )))

# Combine the training and the test sets by row binding
CombineSubject <- rbind(SubjectTrain, SubjectTest)
setnames(CombineSubject, "V1", "subject")
CombineActivity<- rbind(ActivityTrain, ActivityTest)
setnames(CombineActivity, "V1", "activityNum")

#Combine the DATA training and test files
DataTable <- rbind(DataTrain, DataTest)

# name variables according to feature
DataFeatures <- tbl_df(read.table(file.path(filesPath, "features.txt")))
setnames(DataFeatures, names(DataFeatures), c("featureNum", "featureName"))
colnames(DataTable) <- DataFeatures$featureName

#column names for activity labels
activityLabels<- tbl_df(read.table(file.path(filesPath, "activity_labels.txt")))
setnames(activityLabels, names(activityLabels), c("activityNum","activityName"))

# Merge columns, with result as DataSet
DataSet <- cbind(cbind(CombineSubject , CombineActivity), DataTable)

######################################################################################

##2) Extracts only the measurements on the mean and standard deviation for each measurement#


# extracting only the mean and standard deviation from "features.txt" 
dataFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",DataFeatures$featureName,value=TRUE) #var name

# Taking only measurements for the mean and standard deviation and add "subject","activityNum"

dataFeaturesMeanStd <- union(c("subject","activityNum"), dataFeaturesMeanStd)
DataSet <- subset(DataSet,select=dataFeaturesMeanStd) 

######################################################################################

######3) Uses descriptive activity names to name the activities in the data set#######

##enter name of activity into DataSet
DataSet <- merge(activityLabels, DataSet , by="activityNum", all.x=TRUE)
DataSet$activityName <- as.character(DataSet$activityName)

## create dataTable with variable means sorted by subject and Activity
DataSet$activityName <- as.character(DataSet$activityName)
dataAggr<- aggregate(. ~ subject - activityName, data = DataSet, mean) 
DataTable<- tbl_df(arrange(dataAggr,subject,activityName))
######################################################################################

#########4) Appropriately labels the data set with descriptive variable names#########
        
names(DataTable)<-gsub("std()", "SD", names(DataTable))
names(DataTable)<-gsub("mean()", "MEAN", names(DataTable))
names(DataTable)<-gsub("^t", "time", names(DataTable))
names(DataTable)<-gsub("^f", "frequency", names(DataTable))
names(DataTable)<-gsub("Acc", "Accelerometer", names(DataTable))
names(DataTable)<-gsub("Gyro", "Gyroscope", names(DataTable))
names(DataTable)<-gsub("Mag", "Magnitude", names(DataTable))
names(DataTable)<-gsub("BodyBody", "Body", names(DataTable))
######################################################################################

#5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject#
write.table(DataTable, "TidyData.txt", row.name=FALSE)
######################################################################################