library(plyr)
library(dplyr)

#Download and unzip file
datalink <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if(file.exists("datazip.zip") == F) {download.file(datalink, "datazip.zip", mode = "wb")}
unzip("datazip.zip")
setwd("UCI HAR Dataset")

#Quickly read in test and train sets.
initial<-read.table("train\\X_train.txt", nrows=5)
classes<-sapply(initial,class)
trainset<-read.table("train\\X_train.txt", colClasses=classes)

initial<-read.table("test\\X_test.txt", nrows=5)
classes<-sapply(initial,class)
testset<-read.table("test\\X_test.txt", colClasses=classes)

#Merges the training and the test sets to create one data set.
mergedset <- bind_rows(testset, trainset)

#reading in column names and labeling merged data frame
features<-read.table("features.txt")

#reading in activity labels
activity_labels<-read.table("activity_labels.txt")

#reading in test and train labels
test_labels<-read.table("test\\y_test.txt")
train_labels<-read.table("train\\y_train.txt")

#Selecting rows with only mean and std
mean_and_std_indices <- grep("mean|std",features[,2])

#Merges the training and the test labels.
mergedlabels <- bind_rows(test_labels, train_labels)

#subsetting columns
mean_and_std_set <- select(mergedset, mean_and_std_indices)

#adding varible names
colnames(mean_and_std_set) <- features[mean_and_std_indices,2]

#append column with activity labels
mean_and_std_set <- bind_cols(mean_and_std_set, mergedlabels)

#rename activity column
names(mean_and_std_set)[names(mean_and_std_set) == "V1"] <- "Activity"

#replacing activity code to description found in activity_labels file
mean_and_std_set$Activity <- mapvalues(mean_and_std_set$Activity,as.character(activity_labels[,1]),as.character(activity_labels[,2]))

#freeing memory
rm(initial, classes, testset, trainset, test_labels, train_labels, mean_and_std_indices, mergedlabels, mergedset, features, activity_labels)

#Constructing final summarized table. Grouping by activity and calculating mean of each column
final_table <- mean_and_std_set %>% group_by(Activity) %>% summarise_each(funs(mean))

#saving tidy final table with calculated means grouped by activity
write.table(final_table, file="..\\summarized.txt", row.name=F)
