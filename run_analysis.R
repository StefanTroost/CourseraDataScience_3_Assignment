# Coursera "Data Sience Specialization"
# Assignment "Getting and Cleaning Data"
# Stefan Troost, 22/06/2014
#
# To address the questions in the assignment I have decided to:
# - Not follow the strict order of the questions, as this turns out to be much simpler when merging the training set and test set
# Reason for this is that the merge method changes the order of the data (see also remarks of David Hood at https://class.coursera.org/getdata-004/forum/thread?thread_id=106)
# - Keep the train and test characteristics of both datasets in the merged dataset with the field "purpose" (= "train" or "test"). I think this would make sense when using the data for later analysis
# This implies that the aggregation in the final dataset is done on activity, subject and also purpose
# 
#
#setwd("D:/Stefan/Cursus/Specialization Data Science/3 Getting and Cleaning Data/Assignment/UCI HAR Dataset/")
library(data.table)

# Read the training dataset and test dataset
# It is assumed here that there is a folder structure with the datasets in the subfolders /train and /test respectively
# This is actually the structure you get when unzipping the assignments zipfile
trainingSet<-read.table("./train/X_train.txt", header=FALSE)
testSet<-read.table("./test/X_test.txt", header=FALSE)

# Read the feature names from the file features.txt and set them as column names for both the training set and test set
features<-read.table("features.txt", header=FALSE)
featureNames<-as.character(features[,2])
setnames(trainingSet,featureNames)
setnames(testSet,featureNames)

# Enrich both sets with activity info
trainActivities<-read.table("./train/y_train.txt", header=FALSE)
setnames(trainActivities,"activity")
trainingSet<-cbind(trainActivities,trainingSet)
testActivities<-read.table("./test/y_test.txt", header=FALSE)
setnames(testActivities,"activity")
testSet<-cbind(testActivities,testSet)

# Enrich both sets with subject info
trainSubjects<-read.table("./train/subject_train.txt", header=FALSE)
setnames(trainSubjects,"subject")
trainingSet<-cbind(trainSubjects,trainingSet)
testSubjects<-read.table("./test/subject_test.txt", header=FALSE)
setnames(testSubjects,"subject")
testSet<-cbind(testSubjects,testSet)

# Enrich both sets with purpose info
trainingSet$purpose<-"train"
testSet$purpose<-"test"

# Bind the training set and testset with rbind
bothSets<-rbind(trainingSet,testSet)

# Now merge both sets with the activityLabels set
activityLabels<-read.table("activity_labels.txt", header=FALSE)
setnames(activityLabels,c("activity","activityName"))
bothSets<-merge(activityLabels,bothSets)

# Extract only the measurements on the mean and standard deviation for each observation 
bothSetsEnrichingInfo<-bothSets[,c("purpose","subject","activity")]
bothSetsMean<-bothSets[,which(grepl("mean()",names(bothSets),fixed=TRUE))]
bothSetsStd<-bothSets[,which(grepl("std()",names(bothSets),fixed=TRUE))]
bothSetsSelection<-cbind(bothSetsEnrichingInfo,bothSetsMean,bothSetsStd)

# Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
# This file can be read with a simple read.table statement
library(reshape2)
bothSetsSelectionMelt<-melt(bothSetsSelection,id=c("activity","subject","purpose"))
bothSetsSelectionAgg<-dcast(bothSetsSelectionMelt,activity+subject+purpose~variable, mean)
bothSetsSelectionMeltAgg<-melt(bothSetsSelectionAgg,id=c("activity","subject","purpose"))
write.table(bothSetsSelectionMeltAgg,"mergedAveragedTidyData.txt",row.names=FALSE,quote=FALSE)
