# Go to the diretory UCI Har Dataset. I assume we are in a directory above
# Read features file in order to exclude all observations not containing the
# word mean or std
setwd("UCI HAR Dataset")
feat = read.table("features.txt", header=F)
mean_vec = sapply(feat$V2, function(x) length(grep("mean",x,ignore.case=T)) > 0)
std_vec = sapply(feat$V2, function(x) length(grep("std",x,ignore.case=T)) > 0 )

# Go to test dataset and read the test dataset
setwd("test")
te1 <- read.table("X_test.txt", header=FALSE,sep="")
names (te1) <- feat$V2
te1 <- te1[,feat[mean_vec|std_vec,]$V1]
tey <- read.table("y_test.txt", header = FALSE, sep="")

# Read subject file into tes
tes <- read.table("subject_test.txt", header=FALSE, sep="")

# Same for training data
setwd("../train")
tr1 <- read.table("X_train.txt", header=FALSE,sep="")
names(tr1) <- feat$V2
trs <- read.table("subject_train.txt", header=FALSE, sep="")
tr1 <- tr1[,feat[mean_vec|std_vec,]$V1]
tr_y <- read.table("y_train.txt", header = FALSE, sep="")
setwd("..")

# Join test and training data
alldata<- rbind(te1, tr1)
allsubs <- rbind(tes, trs)
ally <- rbind (tey, tr_y)

# Set column names
names(allsubs) <- c("Subject")


ally$activity[ally$V1 == 1] <- "WALKING"
ally$activity[ally$V1 == 2] <- "WALKING_UPSTAIRS"
ally$activity[ally$V1 == 3] <- "WALKING_DOWNSTAIRS"
ally$activity[ally$V1 == 4] <- "SITTING"
ally$activity[ally$V1 == 5] <- "STANDING"
ally$activity[ally$V1 == 6] <- "LAYING"

# Join subject and test data into 1 data frame

all <- cbind(allsubs, ally$activity, alldata)
colnames(all)[2] <- "activity"


tidyData <- aggregate(all[,3:NCOL(all)], list(subject = all$Subject, activity = all$activity), mean )

write.table(tidyData, "tidydata.txt", row.names=FALSE, quote=FALSE)
