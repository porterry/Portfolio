#The goal of this problem is to figure out if credit applicants are a good credit risk or not. 
#I used logistic regression to see predict what category they would fall in. The dataset is from UCI 
#and it is comprised of german credit card applicants. 


library(caret)
library(tidyverse)
library(pROC)

credit_data = read.table("germancredit.txt.", 
                         sep="", 
                         fill=FALSE, 
                         strip.white=TRUE,
                         header = FALSE)

head(credit_data)

#convert 1&2 to 0&1
credit_data$V21[credit_data$V21==1] <- 0
credit_data$V21[credit_data$V21==2] <- 1

#train and test datasets
credit_index <- createDataPartition(credit_data$V21, times = 1, p = 0.7, list = FALSE)
credit_train <- credit_data[credit_index,]
credit_test <- credit_data[-credit_index,]

#logistic regression model
credit_glm <- glm(V21 ~., 
                  family = binomial(link='logit'), #logistic into linear model
                  data = credit_train)

summary(credit_glm)
AIC(credit_glm)

#We start off by converting the response variable to 0 and 1. Then after that we split the data into train and test 
#set to run our models on. We just run a simple linear regression and see that out AIC score is higher than last weeks 
#score (658). We arent going to do any variable selection in this model so it will have to do.

yhat <- predict(credit_glm, credit_test, type="response")

roc(credit_test$V21, round(yhat))

thresh <- 0.7 #threshold
yhat_thresh <- as.integer(yhat > thresh)
conf_matrix <- as.matrix(table(yhat_thresh, credit_test$V21)) #confusion matrix
conf_matrix

cost <- 202*(-1) + 66*(5) + 7*(1) + 25*(0)
cost

# accuracy
accuracy <- (conf_matrix[1,1] + conf_matrix[2,2]) / sum(conf_matrix)
accuracy

# sensitivity
sensitivity <- (conf_matrix[1,1]) / (conf_matrix[1,1] + conf_matrix[2,1])
sensitivity

#For coming up with a threshold it was a game of guess and check method. I was looking for thresholds that would 
#make my cost function low and my accuracy as high as possible without overfitting. We see that the area under the 
#curve is 0.6796. The confusion matrix shows us the classfication of the responses which we'll use to see how accurate 
#we are. Using the confusion matrix we can calculate the cost function, I looked up how to do the cost function so hopefully 
#it's accruate. But you can see that '66*(5)' is accounting for the 5 times worse if a bad customer is identified as good. 
#So those 66 people are the ones that are missclassifed as good credit users when in reality it is the opposite. 
#The accuracy is 0.75 which isnt too bad but still a lot of room for improvement. I want to be somewhere in the 80% but 
#I couldn't find a combination that would give me that. This is where feature selection would have made that 80% possible. 
#Our sensitivity is really high, 0.9665, but that would be at the cost of accuracy. 