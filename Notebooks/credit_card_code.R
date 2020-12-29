library(kernlab)
library(kknn)
library(tidyverse)
library(caret)


data = read.table("credit_card_data.txt.", 
                  sep="", 
                  fill=FALSE, 
                  strip.white=TRUE)

#Use kknn function to find a good classifier:
#(a)	using cross-validation
#(b)	splitting the data into training, validation, and test data sets.

set.seed(0222)
index <- createDataPartition(data$V11, times = 1, p= 0.7, list = FALSE)
train_data <- data[index,] 
test_data <- data[-index,] 

#max number of k's to test
#anything past 30 took a long time 
kmax <- 30

#used the train.kknn for cross validation method
train_model <- train.kknn(V11~.,
                          train_data,
                          kmax=kmax,
                          kernal="optimal",
                          scale=TRUE)

# create an empty array to avoid errors
train_acc <- rep(0,kmax)

# calculate prediction qualities
for (k in 1:kmax) {
  predicted <- as.integer(fitted(train_model)[[k]][1:nrow(train_data)] + 0.5) 
  #like last week you need to round up or down
  train_acc[k] <- sum(predicted == train_data$V11)
}

# show accuracies
train_acc
which.max(train_acc) #k=14

#I first split the data into a test and train set using the createDatapartition which is contained under the caret package. 
#Creates a index that contains 70% of the data which I then put into train_data. I used the train.kknn method for cross validation 
#and the train_acc is a list of the accuracies for each of the k values. When running the code the best k value for me was 14. 
#The R markdown code might have slightly different values since createDataPartition will make different sets each time 
#but set the seed each time to hopefully eliminate that. 

set.seed(0222)
test_model <- train.kknn(V11~.,
                         test_data,
                         ks=14, #based on the train model
                         kernal="optimal",
                         scale=TRUE)

#test the model agaist the test data
test_predicted <- round(predict(test_model,test_data)) 
# round off to 0 or 1
sum(test_predicted == test_data$V11)/length(test_data$V11) #93.4%

#According to the train model, k=14 had the best accuracy so for the test model we used that k value. 
#Using the test model we then predict the value and test the accuracy of said value. 
#Which in this case gave me a 93.4% accuracy which is surprisingly good. I would have assumed to be less than that, 
#but it seems that our k value was a good predictor.

## Run the model again but avoiding overfitting ##

index <- createDataPartition(data$V11, times = 1, p= 0.7, list = FALSE)
#create an index with 70% of the data
train_data <- data[index,] 
data_2 <- data[-index,] 
index2 <- createDataPartition(data_2$V11, times = 1, p= 0.5, list = FALSE)
#split the remanding 30% of data in half
test_data <- data_2[index2,]
validation <- data_2[-index2,]


predicted_train<- rep(0,(nrow(train_data)))
train_acc<- 0  
X<- 0 
#I set all variables to avoid later errors
accTB <-data.frame(matrix(nrow = 30, ncol = 2))
#make an empty df for the k vlaues with accuracies
colnames(accTB) <- c("K","Acc") 


for(X in 1:30){
  
  for (i in 1:nrow(train_data)){
    model=kknn(V11~.,
               train_data[-i,],
               train_data[i,],
               k=X,
               kernel="optimal", 
               scale = TRUE) #scale data
    predicted_train[i]<- as.integer(fitted(model)+0.5) # round off 
  }
  
  # calculate fraction of correct predictions
  train_acc<- sum(predicted_train == train_data[,11]) / nrow(train_data)
  
  accTB[X, 1] <- X
  accTB[X, 2] <- train_acc
}

#Output K accuracy table.
accTB

#The top setup is very similar to to the test data, were I am just creating empty variables and data frames. 
#Then I go through the model like the train model but testing against k values 12 through 17 to validate their accuracies. 
#I create a table again with the values which shows that 12 and 13 perform similarly and 14-17 have the same slightly lower values. 
#For the test set we are going to use the k value of 14 since it had the better preformance. 

predicted_test<- rep(0,(nrow(test_data))) 
# make an empty vector of values
test_acc<- 0 

for (i in 1:nrow(test_data)){
  model=kknn(V11~ .,
             test_data[-i,],
             test_data[i,],
             k=12,
             kernel="optimal", 
             scale = TRUE) # scale data
  predicted_test[i]<- as.integer(fitted(model)+0.5) 
  # round off to 0 or 1 and store predicted values in vector
}

# calculate fraction of correct predictions
test_acc<- sum(predicted_test == test_data[,11]) / nrow(test_data)

train_acc #82.97%
test_acc #77.55%

#Once again we set the variables to zero before running the loop. We only have to calculate one prediction accuracy 
#since we set the k value to 14. Then we compare our test accuracy against the train accuracy. Which the train 
#was 85.15% and the test was 84.69%, we expect the test value to be lower but it wasn't by much. 
#I would say that k=14 gives us the best prediction for our data set. 