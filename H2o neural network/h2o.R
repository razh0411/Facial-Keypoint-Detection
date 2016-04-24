############ 1. training dataset
# training dataset: check missing value for each keypoints and sd
df.train <- read.csv("/Users/ran/Desktop/MA 676/Data Competition/training.csv",stringsAsFactors=F)
summary(df.train)
library(plyr)
colwise(sd)(na.omit(df.train[,1:30]))

# delete missing value and check dim (2140,31)
library(doMC)
registerDoMC()
row.has.na <- apply(df.train, 1, function(x){any(is.na(x))})
sum(row.has.na)
filtered.df.train<-df.train[!row.has.na,] # NO MISSING (2140 rows and 31 cols)

# Get all image data (field) into another variable
im.train<-filtered.df.train$Image
# Remove image data from filtered data
filtered.df.train$Image<-NULL
# But, introduce an ID column for later merger
filtered.df.train$id<-1:dim(filtered.df.train)[1]

# pick up the image data from the training dataset
im.train <- foreach(im = im.train, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}

# Check structure: matrix
class(im.train)
# Convert it to a data frame = 2140 rows and 9216 cols
df.im.train<-data.frame(im.train)
# Remove row names
row.names(df.im.train)<-NULL

# Add an ID to this image data
df.im.train$id<-1:dim(df.im.train)[1]
# Just check what default column names this data has
colnames(df.im.train)

# Merge now complete cases filtered data (30 columns) 
#   with corresponding image data 
#     Merger is on ID column
#       Then remove ID column and save the data frame to hard disk (2140, 9247)
df <- data.frame(merge(filtered.df.train,df.im.train,by="id")) 
df$id<-NULL
dim(df) # 2140, 9246
write.csv(df,"new_train.csv",row.names=F,quote=F)
write.table(df, "Desktop/h2o/new_train.csv", sep="\t")

########## 2. test dataset
library(doMC)
registerDoMC()
df.test<-read.csv("/Users/ran/Desktop/MA 676/Data Competition/test.csv",stringsAsFactors=F,sep="\t")
im.test    <- foreach(im = df.test$Image, .combine=rbind) %dopar% {
  as.integer(unlist(strsplit(im, " ")))
}
df.im.test<-data.frame(im.test)
row.names(df.im.test)<-NULL
write.csv(df.im.test,"new_test.csv",row.names=F,quote=F)
write.table(df.im.test, "Desktop/h2o/new_test.csv", sep="\t")

######## 3. h2o deep learning 
library(h2o)
# Start h2o from within R
localH2O <- h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, max_mem_size = '12g', min_mem_size = '4g', nthreads = -1)
# Loading new train and test files
new_train <- read.csv("Desktop/h2o/new_train.csv",sep="\t")
# In order to save time, we just random 100 from the new train as an example
set.seed(1)
new_train <- new_train[sample(nrow(new_train),100),]
write.csv(new_train,"new_train100.csv",row.names=F,quote=F)
write.table(new_train, "Desktop/h2o/new_train100.csv", sep="\t")
# new test
new_test <- read.csv("Desktop/h2o/new_test.csv",sep="\t")

# Record the time 
begin <- Sys.time()
# Convert test data frame to h2o format
test.hex <- as.h2o(new_test,destination_frame='new_test')

# Initialse data frame with as many rows as are in 'test' to store results
#  Predicted response columns will be appended to this data frame
result<-data.frame(1:dim(new_test)[1])

# Make predictions for columns from 'start' to 'end' one by one
# (Total columns 30)
start<-1 # Start from attribute 1
end<-5     # End at attribute 5

for ( i in start:end )
{
  col<-1:30
  col<-col[-i]
  # Filter columns from training set accordingly
  part_train<-new_train[,-col]
  
  # Convert the training data frame to h2o format
  print("Convert part of csv data to h2o format")
  part.hex <- as.h2o(part_train,destination_frame = 'part_train')
  # Print the column number immediately (flush.console)
  print(i)
  flush.console()
  
  # Start modeling process
  c_name<-paste("Modeling for",names(part_train)[1],sep="")
  # Print column name being modeled
  print(c_name)
  flush.console()
  
  # epoch is a learning cycle or one pass.
  # Training your network on each obs of the set once is an epoch. 
  model <- h2o.deeplearning(x = 2:9217, y = 1, training_frame=part.hex, nfolds = 10, l1=1e-5 ,  activation = "RectifierWithDropout", input_dropout_ratio = 0.2, hidden_dropout_ratios = c(0.5,0.5,0.5,0.5,0.5,0.5), hidden = c(200,200,100,100,50,50), epochs = 40)
  # epochs = 40 how many times the dataset should be iterated
  print("Modeling completed")
  flush.console()
  
  ## Predictions
  # In test data frame, make predictions for this column
  test_predict.hex <- h2o.predict(model, test.hex)
  # Transform it to dataframe format
  test_predict <- as.data.frame(test_predict.hex)
  # Change column name of test_predict to that of response column
  colnames(test_predict)=names(part_train)[1]
  
  # Append predicted response column to result dataframe
  result[i-start+1]<-test_predict
  
  # Write every result to file (sample file name is: first5.csv)
  result_file<-paste("first",end,".csv",sep="")
  write.csv(result, file = result_file , row.names=FALSE, quote=FALSE)
  # Remove garbage & release memory to OS. 
  gc()
}

# Analysis Ending time
over <- Sys.time()
timeused <- over - begin 
timeused

############ results
# result first 5: time 5.16 hrs
sub1<-read.csv("first5.csv",header=T)  
dim(sub1)
write.csv(result, file = result_file, row.names=FALSE, quote=FALSE)
write.table(result, "Desktop/result.csv", row.names=FALSE, quote=FALSE)

# result 6-7
sub2<-read.csv("first7.csv",header=T)  
dim(sub2)
write.csv(result, file = result_file, row.names=FALSE, quote=FALSE)
write.table(result, "Desktop/result(6-7).csv", row.names=FALSE, quote=FALSE)

# result 8-10
sub3<-read.csv("first10.csv",header=T)  
dim(sub3)
write.csv(result, file = result_file, row.names=FALSE, quote=FALSE)
write.table(result, "Desktop/result(8-10).csv", row.names=FALSE, quote=FALSE)

# result 11-12: time 19.11 mins (right_eye_outer_corner_x and right_eye_outer_corner_y)
sub4<-read.csv("first12.csv",header=T)  
dim(sub4)
write.csv(result, file = result_file, row.names=FALSE, quote=FALSE)
write.table(result, "Desktop/result(11-12).csv", row.names=FALSE, quote=FALSE)

# result 13-15: time 27 mins (left_eyebrow_inner_end_x,left_eyebrow_inner_end_y, and left_eyebrow_outer_end_x)
sub5<-read.csv("first15.csv",header=T)  
dim(sub5)
write.csv(result, file = result_file, row.names=FALSE, quote=FALSE)
write.table(result, "Desktop/result(13-15).csv", row.names=FALSE, quote=FALSE)

# result 16-20: time 45.2638 mins
# left_eyebrow_outer_end_y, right_eyebrow_inner_end_x, right_eyebrow_inner_end_y,
# right_eyebrow_outer_end_x and right_eyebrow_outer_end_y
sub6<-read.csv("first20.csv",header=T)  
dim(sub6)
write.csv(result, file = result_file, row.names=FALSE, quote=FALSE)
write.table(result, "Desktop/result(16-20).csv", row.names=FALSE, quote=FALSE)

# result 21-25
sub7<-read.csv("first25.csv",header=T)  
dim(sub7)
write.csv(result, file = result_file, row.names=FALSE, quote=FALSE)
write.table(result, "Desktop/result(21-25).csv", row.names=FALSE, quote=FALSE)

# result 26-30: time 45.31037 mins
# mouth_right_corner_y,mouth_center_top_lip_x, mouth_center_top_lip_y,
# mouth_center_bottom_lip_x,and mouth_center_bottom_lip_y
sub8<-read.csv("first30.csv",header=T)  
dim(sub8)
write.csv(result, file = result_file, row.names=FALSE, quote=FALSE)
write.table(result, "Desktop/result(26-30).csv", row.names=FALSE, quote=FALSE)
