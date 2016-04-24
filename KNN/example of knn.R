library(foreach)
library(reshape2)
library(psych)
library(class)
setwd("C:/Users/Administrator/Desktop/kaggle")
d.train<- read.csv("training.csv",stringsAsFactors=F)
setwd("C:/Users/Administrator/Desktop/kaggle")
d.test <- read.csv("test.csv",stringsAsFactors=F)

# No missing value of training data
row.has.na <- apply(d.train, 1, function(x){any(is.na(x))})
sum(row.has.na)
d.train.s <- d.train[!row.has.na,] 

# Image data of training data with no missing value
im.train.s   <- foreach(im = d.train.s $Image, .combine=rbind) %do% {
  as.integer(unlist(strsplit(im, " ")))
} 

# Image data of test data with no missing value
im.test <- foreach(im = d.test$Image, .combine=rbind)  %do% {
  as.integer(unlist(strsplit(im, " ")))
}

# K, we should use
sqrt(nrow(im.train.s))

#left_eye_outer_corner_y
im_train_labels.leocy <- d.train.s$left_eye_outer_corner_y
knn.pred.leocy=knn(im.train.s,im.test,im_train_labels.leocy,k=47)
knn.result.leocy <- as.data.frame(knn.pred.leocy)
write.table(knn.result.leocy, "C:/Users/Administrator/Desktop/kaggle result/knn_leocy", row.names = TRUE,
            col.names = TRUE)