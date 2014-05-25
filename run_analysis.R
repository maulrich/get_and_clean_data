# Working Path is set to "C:/UCI HAR Dataset/

# Checking dependend packages
if (!require("data.table")) {
  install.packages("data.table")
  require("data.table")
}
if (!require("reshape2")) {
  install.packages("reshape2")
  require("reshape2")
}

# data loader: loads data fpr test and train
data_loader <- function(file_x, file_y, file_s, features, activity) {
  # labels to process
  features_tmp <- grepl("mean|std", features)  
  # reading data file
  x <- read.table(file_x)
  names(x) = features  
  # selecting mean and std cols
  x = x[,features_tmp]  
  # activities
  y <- read.table(file_y)
  y[,2] = activity[y[,1]]
  names(y) = c("Activity_ID", "Activity_Label")  
  # subjects
  subject <- read.table(file_s)
  names(subject) = "subject"  
  # put it together
  cbind(as.data.table(subject), y, x)
}

data_path = "C:/UCI HAR Dataset/"
# reading data cols
features <- read.table(paste(data_path,"features.txt", sep="")) [,2]
# reading activ labels
activity <- read.table(paste(data_path,"activity_labels.txt", sep="")) [,2]

# reading data with data_loader
test_data = data_loader(file_x = paste(data_path,"test/X_test.txt", sep=""), file_y = paste(data_path,"test/y_test.txt", sep=""), file_s = paste(data_path,"test/subject_test.txt", sep=""), features, activity)
train_data = data_loader(file_x = paste(data_path,"train/X_train.txt", sep=""), file_y = paste(data_path,"train/y_train.txt", sep=""), file_s = paste(data_path,"train/subject_train.txt", sep=""), features, activity)

# binding test and train_data
dat = rbind(test_data, train_data)

# Group and aggregate by label
label_ids = c("subject", "Activity_ID", "Activity_Label")
data_labels = setdiff(colnames(dat), label_ids)
redata = melt(dat, id = label_ids, measure.vars = data_labels)
# calc mean
tidy_data = dcast(redata, subject + Activity_Label ~ variable, mean)
#write tble
write.table(tidy_data, file = paste(data_path,"tidy_data.txt", sep=""))


