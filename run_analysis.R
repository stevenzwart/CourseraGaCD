setwd("UCI HAR Dataset/test")
# Read Test table into te1
te1 <- read.table("X_test.txt", header=FALSE,sep="")
# Read subject file into tes
tes <- read.table("subject_test.txt", header=FALSE, sep="")
# Select only columns with mean and standard deviation
te2 <- te1[,c(1:6,41:46,81:86,121:126,161:166,201:202, 214:215, 227:228, 240:241, 253:254, 266:271
              , 345:350, 424:429, 503:504, 516:517, 529:530, 542:543 )]
# Same for training data
setwd("../train")
tr1 <- read.table("X_train.txt", header=FALSE,sep="")
trs <- read.table("subject_train.txt", header=FALSE, sep="")
tr2 <- tr1[,c(1:6,41:46,81:86,121:126,161:166,201:202, 214:215, 227:228, 240:241, 253:254, 266:271
            , 345:350, 424:429, 503:504, 516:517, 529:530, 542:543 )]
            
# Join test and training data
t2 <- rbind(te2, tr2)
ts <- rbind(tes, trs)

# Set column names
names(ts) <- c("Subject")

names(t2) <- c('tBodyAccMeanX'
               , 'tBodyAccMeanY' 
               , 'tBodyAccMeanZ'
               , 'tBodyAccSTDX'
               , 'tBodyAccSTDY'
               , 'tBodyAccSTDZ'
               , 'tGravityAccMeanX'
               , 'tGravityAccMeanY'
               , 'tGravityAccMeanZ'
               , 'tGravityAccSTDX'
               , 'tGravityAccSTDY'
               , 'tGravityAccSTDZ'
               , 'tBodyAccJerkMeanX'
               , 'tBodyAccJerkMeanY'
               , 'tBodyAccJerkMeanZ'
               , 'tBodyAccJerkSTDX'
               , 'tBodyAccJerkSTDY'
               , 'tBodyAccJerkSTDZ'
               , 'tBodyGyroMeanX'
               , 'tBodyGyroMeanY'
               , 'tBodyGyroMeanZ'
               , 'tBodyGyroSTDX'
               , 'tBodyGyroSTDY'
               , 'tBodyGyroSTDZ'
               , 'tBodyGyroJerkMeanX'
               , 'tBodyGyroJerkMeanY'
               , 'tBodyGyroJerkMeanZ'
               , 'tBodyGyroJerkSTDX'
               , 'tBodyGyroJerkSTDY'
               , 'tBodyGyroJerkSTDZ'
               , 'tBodyAccMagMean'
               , 'tBodyAccMagStd'
               , 'tGravityAccMagMean'
               , 'tGravityAccMagStd'
               , 'tBodyAccJerkMagMean'
               , 'tBodyAccJerkMagStd'
               , 'tBodyGyroMagMean'
               , 'tBodyGyroMagStd'
               , 'tBodyGyroJerkMagMean'
               , 'tBodyGyroJerkMagStd'
               , 'fBodyAccMeanX'
               , 'fBodyAccMeanY'
               , 'fBodyAccMeanZ'
               , 'fBodyAccSTDX'
               , 'fBodyAccSTDY'
               , 'fBodyAccSTDZ'
               , 'fBodyAccJerkMeanX'
               , 'fBodyAccJerkMeanY'
               , 'fBodyAccJerkMeanZ'
               , 'fBodyAccJerkSTDX'
               , 'fBodyAccJerkSTDY'
               , 'fBodyAccJerkSTDZ'
               , 'fBodyGyroMeanX'
               , 'fBodyGyroMeanY'
               , 'fBodyGyroMeanZ'
               , 'fBodyGyroSTDX'
               , 'fBodyGyroSTDY'
               , 'fBodyGyroSTDZ'
               , 'fBodyAccMagMean'
               , 'fBodyAccMagStd'
               , 'fBodyBodyAccJerkMagMean'
               , 'fBodyBodyAccJerkMagStd'
               , 'fBodyBodyGyroMagMean'
               , 'fBodyBodyGyroMagStd'
               , 'fBodyBodyGyroJerkMagMean'
               , 'fBodyBodyGyroJerkMagStd')


# Join subject and test data into 1 data frame

newData <- cbind(ts, t2)

# Calculate means per subject for all the "mean" variables (not the standard deviations)

tidyData <- ddply(newData,~Subject,summarise,BodyAccMeanX=mean(tBodyAccMeanX)
                  ,BodyAccMeanY=mean(tBodyAccMeanY)
                  , tBodyAccMeanZ=mean(tBodyAccMeanZ)
                  , tGravityAccMeanX=mean(tGravityAccMeanX)
                  , tGravityAccMeanY=mean(tGravityAccMeanY)
                  , tGravityAccMeanZ=mean(tGravityAccMeanZ)
                  , tBodyAccJerkMeanX=mean(tBodyAccJerkMeanX)
                  , tBodyAccJerkMeanY=mean(tBodyAccJerkMeanY)
                  , tBodyAccJerkMeanZ=mean(tBodyAccJerkMeanZ)
                  , tBodyGyroMeanX=mean(tBodyGyroMeanX)
                  , tBodyGyroMeanY=mean(tBodyGyroMeanY)
                  , tBodyGyroMeanZ=mean(tBodyGyroMeanZ)
                  , tBodyGyroJerkMeanX=mean(tBodyGyroJerkMeanX)
                  , tBodyGyroJerkMeanY=mean(tBodyGyroJerkMeanY)
                  , tBodyGyroJerkMeanZ=mean(tBodyGyroJerkMeanZ)
                  , tBodyAccMagMean=mean(tBodyAccMagMean)
                  , tGravityAccMagMean=mean(tGravityAccMagMean)
                  , tBodyAccJerkMagMean=mean(tBodyAccJerkMagMean)
                  , tBodyGyroMagMean=mean(tBodyGyroMagMean)
                  , tBodyGyroJerkMagMean=mean(tBodyGyroJerkMagMean)
                  , fBodyAccMeanX=mean(fBodyAccMeanX)
                  , fBodyAccMeanY=mean(fBodyAccMeanY)
                  , fBodyAccMeanZ=mean(fBodyAccMeanZ)
                  , fBodyAccSTDX=mean(fBodyAccSTDX)
                  , fBodyAccSTDY=mean(fBodyAccSTDY)
                  , fBodyAccSTDZ=mean(fBodyAccSTDZ)
                  , fBodyAccJerkMeanX=mean(fBodyAccJerkMeanX)
                  , fBodyAccJerkMeanY=mean(fBodyAccJerkMeanY)
                  , fBodyAccJerkMeanZ=mean(fBodyAccJerkMeanZ)
                  , fBodyGyroMeanX=mean(fBodyGyroMeanX)
                  , fBodyGyroMeanY=mean(fBodyGyroMeanY)
                  , fBodyGyroMeanZ=mean(fBodyGyroMeanZ)
                  , fBodyAccMagMean=mean(fBodyAccMagMean)
                  , fBodyAccMagStd=mean(fBodyAccMagStd)
                  , fBodyBodyAccJerkMagMean=mean(fBodyBodyAccJerkMagMean)
                  , fBodyBodyGyroMagMean=mean(fBodyBodyGyroMagMean)
                  , fBodyBodyGyroJerkMagMean=mean(fBodyBodyGyroJerkMagMean)
)
