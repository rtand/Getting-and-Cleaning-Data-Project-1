#You should create one R script called run_analysis.R that does the following. 
#Merges the training and the test sets to create one data set.
#Extracts only the measurements on the mean and standard deviation for each measurement. 
#Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive variable names. 
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#Load Libraries, assume installed
library(dplyr)

# Load activity labels and features
	activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt",stringsAsFactors = FALSE)
	activityLabels<-rename(activityLabels,Activity=V1,Name=V2)
	features <- read.table("UCI HAR Dataset/features.txt",stringsAsFactors = FALSE)
	features<-rename(features,ID=V1,Name=V2)

# Extract only the data on mean and standard deviation
	features<-filter(features, grepl('mean|std', Name))
	features$Name <- gsub('[-()]', '', features$Name) #get rid of the ugly () in the col name
	features$Name <- gsub('mean', 'MEAN', features$Name) #lets change the lowercase mean to caps, for readability
	features$Name <- gsub('std', 'STD', features$Name) #same thing with std

# Load the training files
	train <- read.table("UCI HAR Dataset/train/X_train.txt")[features$ID] #features$ID is the list of columns we found earlier
	trainActivities <- read.table("UCI HAR Dataset/train/Y_train.txt",stringsAsFactors = FALSE)
	trainSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt",stringsAsFactors = FALSE)
	train <- cbind(trainSubjects, trainActivities, train) #this makes the assumption that all files have the same record order

# Load the test files
	test <- read.table("UCI HAR Dataset/test/X_test.txt")[features$ID]
	testActivities <- read.table("UCI HAR Dataset/test/Y_test.txt",stringsAsFactors = FALSE)
	testSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt",stringsAsFactors = FALSE)
	test <- cbind(testSubjects, testActivities, test) #this makes the assumption that all files have the same record order

# Put the testing and training data together into one big table (UNION)
	allData <- rbind(train, test)
	allData<-tbl_df(allData) #not needed, but makes it pretty when looking at the table in R

#Add column names from the activity_labels.txt file
	colnames(allData) <- c("Subject", "Activity", features$Name) #adding column names
	allData<-inner_join(allData,activityLabels,"Activity") #lets add the human readable activity names
	allData<-select(allData,-Activity) #getting rid of the activity ID, we dont need it
	allData<-rename(allData,Activity=Name) #change the column name to keep it consistent

#Creating the summary table
	allData<-group_by(allData,Subject, Activity) #using dplyr grouping
	allDataSummary<-summarise_each(allData,funs(mean)) #get the means of each variable, except the grouping elements (Subject,Activity)

write.table(allDataSummary, "tidy.txt", row.names = FALSE, quote = FALSE) #write the data out to the working directory