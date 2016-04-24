library(reshape2)
library(doMC)
registerDoMC()
new_train <- read.csv("Desktop/h2o/new_train100.csv",sep="\t")
new_test <- read.csv("Desktop/h2o/new_test.csv",sep="\t")

############ Prepare submission file
# Read all prediction files one by one
sub1 <- read.csv("/Users/ran/Desktop/h2o/result(1-5).csv",sep=",")
sub2 <- read.csv("/Users/ran/Desktop/h2o/result(6-7).csv",sep=",")
sub3 <- read.csv("/Users/ran/Desktop/h2o/result(8-10).csv",sep=",")
sub4 <- read.csv("/Users/ran/Desktop/h2o/result(11-12).csv",sep=",")
sub5 <- read.csv("/Users/ran/Desktop/h2o/result(13-15).csv",sep=",")
sub6 <- read.csv("/Users/ran/Desktop/h2o/result(16-20).csv",sep=",")
sub7 <- read.csv("/Users/ran/Desktop/h2o/result(20-25).csv",sep=",")
sub7 <- sub7[,-1]
sub8 <- read.csv("/Users/ran/Desktop/h2o/result(26-30).csv",sep=",")

# Recheck 1. dim
dim(sub1)
dim(sub2)
dim(sub3)
dim(sub4)
dim(sub5)
dim(sub6)
dim(sub7)
dim(sub8)

# 2. colnames
colnames(sub1)
colnames(sub2)
colnames(sub3)
colnames(sub4)
colnames(sub5)
colnames(sub6)
colnames(sub7)
colnames(sub8)

# Merge all data frame we build
submission<-sub1
submission[,6:7]<-sub2
submission[,8:10]<-sub3
submission[,11:12]<-sub4
submission[,13:15]<-sub5
submission[,16:20]<-sub6
submission[,21:25]<-sub7
submission[,26:30]<-sub8
write.table(submission, "Desktop/h2o/predicted.csv",row.names = TRUE, col.names = TRUE)

# Assign col names to 'first' for 30 columns
colnames(submission)<-names(new_train[,1:30])

# Create a data frame with as many rows as in test.
#   ImageId column contains seq number 
predictions <- data.frame(ImageId = 1:nrow(new_test))
predictions[2:31]<-submission         # Add other 30 columns to it
head(predictions)                # Check

# Restack predictions, ImageId wise
subfile <- melt(predictions, id.vars="ImageId", variable.name="FeatureName", value.name="Location")
head(subfile)
# Read IdLookupTable.csv file downoloaded from Kaggle 
Id.lookup <- read.csv("Desktop/MA 676/Data Competition/IdLookupTable.csv",header=T)
Idlookup_colnames <- names(Id.lookup)
Idlookup_colnames
Id.lookup$Location <- NULL

msub <- merge(Id.lookup, subfile, all.x=T, sort=F)
# Adds columns (RowId) not in msub
nsub <- msub[, Idlookup_colnames]

# Save and Kaggle
write.table(nsub,"Desktop/subfile_h2o.csv", row.names=FALSE, quote=FALSE)


########### Plot from test data and predicted location

# 1. original image in test data 
test <- read.csv("Desktop/MA 676/Data Competition/test.csv",stringsAsFactors=F)
im.test <- foreach(im = test$Image, .combine = rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}
im <- matrix(data=rev(im.test[502,]), nrow=96, ncol=96) # 502 image , 1264
image(1:96, 1:96, im, col=gray((0:255)/255))

# 2. The predicted location in the submission file
dim(submission)
for(i in 1:nrow(submission)) {
  points(96-submission$right_eyebrow_inner_end_x[i], 96-submission$right_eyebrow_inner_end_y[i], col="red")
}

# 3. check the maximum of one keypoint and plot the image, then find the location in this image 
idx <- which.max(submission$right_eye_center_y)
im  <- matrix(data=rev(im.test[idx,]), nrow=96, ncol=96)
image(1:96, 1:96, im, col=gray((0:255)/255))
points(96-submission$left_eye_center_x[idx], 96-submission$left_eye_center_y[idx], col="red")

