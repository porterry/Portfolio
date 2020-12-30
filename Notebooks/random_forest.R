#Using the same crime data set from statsci and in previous notebooks, build the best model using regression tree and random forest model.

library(tidyverse)
library(randomForest)
library(tree)
library(rpart.plot)
library(caret)
library(pROC)

#setwd("/Users/Ryan/Desktop/DS")

crime_data = read.table("uscrime.txt.", 
                        sep="", 
                        fill=FALSE, 
                        strip.white=TRUE,
                        header = TRUE)

#Loading in the many packages that will be used throughout the problem. Next is setting the working directory and reading 
#in the crime data that was given to us. Once we build the model, we are trying to predict the crime rate given those values 
#about the city and then see how well our model does compared to the PCA model. 

## Regression Tree ##
crime_tree <- tree(Crime~. , data = crime_data)
summary(crime_tree)

crime_tree$frame #how the tree is split
#there are 7 leaves

#plot the tree
plot(crime_tree)
text(crime_tree)
title('Crime Data using Tree')

prune.tree(crime_tree)$size
prune.tree(crime_tree)$dev

#we shouldnt prune because the deviance is actually getting larger with less leafs
#however this might change after cross validation
#7 leaves: 1895722
#6 leaves: 2013257

#Building the regression tree model is very similar to the linear regression model. Frame shows us how many leaves 
#in the tree we have, this model split it into 7 leaves. Next, we plot the tree function so you can get a sense of where 
#the splits are at. Then we look at the number of leaves (size) and the deviance within those leaves. From the output it 
#seems like 7 leaves gives us the best split, but we will check with corss validation. You can see that the deviance is 
#about 100,000 less with 7 leaves compared to 6 leaves but this might be overfitting. 

set.seed(1982)
prune_tree <- cv.tree(object = crime_tree, FUN = prune.tree)
prune_tree$size
prune_tree$dev

which.min(prune_tree$dev)
# it looks like 6 leaves is the best one after cross validation

crime_tree_prune <- prune.tree(crime_tree, best = 6)
summary(crime_tree_prune)
plot(crime_tree_prune)
text(crime_tree_prune)

prune_predict <- predict(crime_tree_prune, data = crime_data[,1:15])
RSS <- sum((prune_predict - crime_data[,16])^2)
TSS <- sum((crime_data[,16] - mean(crime_data[,16]))^2)
R <- 1 - RSS/TSS
R #0.7074

#First we cross validate the tree and prune it using the prune.tree function. The output is a lot different than when 
#we ran the tree just once. It seems that 6 leaves give us the smallest deviance so we will use 6 leaves going forward.
#Then you take the original model and prune it using six leaves. We plot it again and compute the R-squared value which is 0.7074. 
#Last week I got the value 0.645 so it seems that pruning the tree did better. 

# i used rpart since its a regression tree
rr <- rpart(Crime~., 
            data = crime_data,
            control = (minsplit = seq(2,10,1)))

bestcp <- rr$cptable[which.min(rr$cptable[,"xerror"]),"CP"]
rr.pruned <- prune(rr, cp = bestcp)

rpart.plot(rr)
predict1 <- predict(rr.pruned, data = crime_data[,1:15])
RSS <- sum((predict1 - crime_data[,16])^2)
TSS <- sum((crime_data[,16] - mean(crime_data[,16]))^2)
R2 <- 1 - RSS/TSS
R2 #0.3629

#I also tried the other method for creating a regression tree which is rpart. However, how I determined the fit seemed to 
#not work very well. My R-sqaured dropped significantly so I'll need more time to figure out how to optimize it even better. 

## Random Forest Model ##
num_pred <- 4
#recommendation is 1+log(n) or n/3 where n is the number of predictors 

crime_rf <- randomForest(Crime ~.,
                         data = crime_data,
                         mtry = num_pred,
                         importance = TRUE)
crime_rf

importance(crime_rf)
#Po1, Po2, and NW

rf_predict <- predict(crime_rf, data=crime_data[,-16])
RSS <- sum((rf_predict - crime_data[,16])^2)
R3 <- 1 - RSS/TSS
R3

#The next part of the problem asked us to make a randomForest of the data. I orginally used mtry = 5 but that gave me a much lower R^2 
#(about 0.34) so after trying a few different options decided on 4. From the importance function the best predictors were Po1, Po2, NW, 
#and Prob. We don't need to run cross validation because it is a randomForest.  

