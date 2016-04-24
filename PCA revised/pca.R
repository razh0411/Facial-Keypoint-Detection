library(doMC)
registerDoMC()
library(reshape2)
library(foreach)
##### dataset

d.train <- read.csv("/Users/wangjianing/Documents/Boston U/BU/spring 2016/MA 676/kaggle competition/training.csv",stringsAsFactors=F)
d.test <- read.csv("/Users/wangjianing/Documents/Boston U/BU/spring 2016/MA 676/kaggle competition/test.csv",stringsAsFactors=F)
d.train.s <- d.train[sample(nrow(d.train),1000),]
d.train.s[,1:30]

im.train   <- foreach(im = d.train$Image, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}

im.train.s   <- foreach(im = d.train.s$Image, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}
# im.train for 10 samples, containing 9216 cols and 10 rows

# PCA
train.coord.eye<-d.train.s[,1:12]
str(d.train[,1:31])
rowhasna<-apply(train.coord.eye,1,function(x){any(is.na(x))})
train.coord.eye<-train.coord.eye[!rowhasna,]
summary(princomp(~train.coord.eye[,1]+train.coord.eye[,2]+train.coord.eye[,3]+train.coord.eye[,4]+train.coord.eye[,5]+train.coord.eye[,6]+train.coord.eye[,7]+train.coord.eye[,8]
         +train.coord.eye[,9]+train.coord.eye[,10]+train.coord.eye[,11]+train.coord.eye[,12],cor=T))
# top 4 explain 92.466% variability
# (2.2331441 1.7146847 1.4096023 1.08713896 )/12
principal(train.coord.eye,nfactors=4)
principal(train.coord.eye,nfactors=4,rotate="varimax")
# PC4:right_eye_center_y, right_eye_inner_corner_y,right_eye_outer_corner_y
# PC1:left_eye_center_y ,left_eye_inner_corner_y, left_eye_outer_corner_y 
# PC3 : right_eye_center_x, right_eye_inner_corner_x,right_eye_outer_corner_x 
# PC2: left_eye_center_x, left_eye_inner_corner_x ,left_eye_outer_corner_x 
factanal(x=train.coord.eye,factors=3)

train.coord.m<-d.train.s[,23:30]
str(train.coord.m)
rowhasna.m<-apply(train.coord.m,1,function(x){any(is.na(x))})
im.train.s<-im.train.s[!rowhasna.m,]
train.coord.m<-train.coord.m[!rowhasna.m,]
factanal(x=train.coord.m,factors=4)
# Y corner and bottom coordinates appear to be more consistent in the dataset. ---select
# X corner and y top coordinates appear to be more unique.
##### select  mouth_left_corner_y  mouth_right_corner_y mouth_center_bottom_lip_y
select<-data.frame(train.coord.m$mouth_left_corner_y, train.coord.m$mouth_right_corner_y, train.coord.m$mouth_center_bottom_lip_y)
principal(select,nfactors=3,rotate="varimax")

summary(svm(train.coord.m$mouth_center_bottom_lip_y ~ im.train.s))



### use image data (transfered) to conduct PCA
summary(princomp(~t(im.train.s),cor=T))
aaa<-principal(t(im.train.s),nfactors=36,rotate="varimax")
frame.im.train.s<-data.frame(im.train.s)
mean(d.train[which(aaa$loadings[,1]>0.5),1])
str(aaa)
