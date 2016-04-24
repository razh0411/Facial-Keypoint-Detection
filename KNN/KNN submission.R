knn1 <- read.csv("Desktop/KNN result/knn_lecx.csv",sep=",")
knn2 <- read.csv("Desktop/KNN result/knn_lecy.csv",sep=",")
knn3 <- read.csv("Desktop/KNN result/knn_leicx.csv",sep=",")
knn4 <- read.csv("Desktop/KNN result/knn_leicy.csv",sep=",")
knn5 <- read.csv("Desktop/KNN result/knn_leiex.csv",sep=",")
knn6 <- read.csv("Desktop/KNN result/knn_leiey.csv",sep=",")
knn7 <- read.csv("Desktop/KNN result/knn_leocx.csv",sep=",")
knn8 <- read.csv("Desktop/KNN result/knn_leocy.csv",sep=",")
knn9 <- read.csv("Desktop/KNN result/knn_leoex.csv",sep=",")
knn10 <- read.csv("Desktop/KNN result/knn_leoey.csv",sep=",")
knn11 <- read.csv("Desktop/KNN result/knn_mcbly.csv",sep=",")
knn12 <- read.csv("Desktop/KNN result/knn_mctlx.csv",sep=",")
knn13 <- read.csv("Desktop/KNN result/knn_mctly.csv",sep=",")
knn14 <- read.csv("Desktop/KNN result/knn_mlcx.csv",sep=",")
knn15 <- read.csv("Desktop/KNN result/knn_mlcy.csv",sep=",")
knn16 <- read.csv("Desktop/KNN result/knn_mrcx.csv",sep=",")
knn17 <- read.csv("Desktop/KNN result/knn_mrcy.csv",sep=",")
knn18 <- read.csv("Desktop/KNN result/knn_ntx.csv",sep=",")
knn19 <- read.csv("Desktop/KNN result/knn_nty.csv",sep=",")
knn20 <- read.csv("Desktop/KNN result/knn_recx.csv",sep=",")
knn21 <- read.csv("Desktop/KNN result/mouth_center_bottom_lip_X.csv",sep=",")
knn22 <- read.csv("Desktop/KNN result/knn_recy.csv",sep=",")
knn23 <- read.csv("Desktop/KNN result/knn_reicx.csv",sep=",")
knn24 <- read.csv("Desktop/KNN result/knn_reicy.csv",sep=",")
knn25 <- read.csv("Desktop/KNN result/knn_reiex.csv",sep=",")
knn26 <- read.csv("Desktop/KNN result/knn_reiey.csv",sep=",")
knn27 <- read.csv("Desktop/KNN result/knn_reocx.csv",sep=",")
knn28 <- read.csv("Desktop/KNN result/knn_reocy.csv",sep=",")
knn29 <- read.csv("Desktop/KNN result/knn_reoex.csv",sep=",")
knn30 <- read.csv("Desktop/KNN result/knn_reoey.csv",sep=",")


sub <- cbind(knn1,knn2)
sub1<-cbind(sub,knn3)
sub2 <-cbind(sub1,knn4)
sub3 <-cbind(sub2,knn5)
sub4 <-cbind(sub3,knn6)
sub5 <-cbind(sub4,knn7)
sub6 <-cbind(sub5,knn8)
sub7 <-cbind(sub6,knn9)
sub8 <-cbind(sub7,knn10)
sub9 <-cbind(sub8,knn11)
sub10 <-cbind(sub9,knn12)
sub11 <-cbind(sub10,knn13)
sub12 <-cbind(sub11,knn14)
sub13 <-cbind(sub12,knn15)
sub14 <-cbind(sub13,knn16)
sub15 <-cbind(sub14,knn17)
sub16 <-cbind(sub15,knn18)
sub17 <-cbind(sub16,knn19)
sub18 <-cbind(sub17,knn20)
sub19 <-cbind(sub18,knn21)
sub20 <-cbind(sub19,knn22)
sub21 <-cbind(sub20,knn23)
sub22 <-cbind(sub21,knn24)
sub23 <-cbind(sub22,knn25)
sub24 <-cbind(sub23,knn26)
sub25 <-cbind(sub24,knn27)
sub26 <-cbind(sub25,knn28)
sub27 <-cbind(sub26,knn29)
sub28 <-cbind(sub27,knn30)

submission <- sub28
dim(submission)
write.table(submission,"Desktop/KNN result/predicted file.csv",row.names=FALSE, quote=FALSE)




# Create a data frame with as many rows as in test.
#   ImageId column contains seq number 
test <- read.csv("Desktop/MA 676/Data Competition/test.csv",stringsAsFactors=F)
submission <- read.csv("Desktop/KNN result/predicted file.csv",sep=",")
predictions <- data.frame(ImageId = 1:nrow(test))
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
write.table(nsub,"Desktop/submission.csv", row.names=FALSE, quote=FALSE)


### plot
library(doMC)
registerDoMC()

# 1. original image in test data 
test <- read.csv("Desktop/MA 676/Data Competition/test.csv",sep=",",stringsAsFactors = F)
colnames(test)
im.test <- foreach(im = test$Image, .combine = rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}
knn.sub <- read.csv("Desktop/KNN result/predicted file.csv",sep=",")
im <- matrix(data=rev(im.test[1264,]), nrow=96, ncol=96) # first image 
image(1:96, 1:96, im, col=gray((0:255)/255))

# 2. The predicted location in the submission file
dim(knn.sub)
for(i in 1:nrow(knn.sub)) {
  points(96-knn.sub$mouth_center_top_lip_x[i], 96-knn.sub$mouth_center_top_lip_y[i], col="red")
}

# 3. check the maximum of one keypoint and plot the image, then find the location in this image 
idx <- which.max(knn.sub$right_eye_center_y)
im  <- matrix(data=rev(im.test[idx,]), nrow=96, ncol=96)
image(1:96, 1:96, im, col=gray((0:255)/255))
points(96-knn.sub$left_eye_center_x[idx], 96-knn.sub$left_eye_center_y[idx], col="red")

