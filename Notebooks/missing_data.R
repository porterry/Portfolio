#Using the breast cancer dataset from UCI, compute the missing values using mean, mode, regression, and regression with perturbation. 
#After the missing values have been imputed, ran a KNN model on the different datasets and compare the qualuty of the classfication.

## Packages & Data
library(tidyverse)
library(mice)
library(caret)

rm(list = ls())

set.seed(123)

#setwd("/Users/Ryan/Desktop/DS")

bcw = read.table("breast-cancer-wisconsin.txt.", 
                 sep=",", 
                 fill=FALSE, 
                 strip.white=TRUE,
                 header = FALSE,
                 stringsAsFactors = FALSE)

bcw2 = read.csv("breast-cancer-wisconsin.txt.", 
                header = FALSE,
                na.strings = '?')

#I loaded in the data two different ways: one treating it as a text file and the other as a csv file. I originally loaded in the 
#data as a text file because that is how the file is saved as. However, I ended up wanting the data values as integers and it is 
#easier to declare the missing values. 

## Explore the Data
head(bcw)
summary(bcw)

#index's of the missing data
impute_me <- which(bcw$V7 == "?")
impute_me

#How much are we missing?
length(impute_me)/length(bcw$V7) # ~2%

#Check out V11 
bcw_clean <- bcw[-impute_me,]
bcw_missing <- bcw[impute_me,]
table(bcw$V11)
table(bcw_clean$V11)
table(bcw_missing$V11) #proportion looks extreme

sum(bcw$V11 == 2)/nrow(bcw)
sum(bcw_clean$V11 == 2)/nrow(bcw_clean)
sum(bcw_missing$V11 == 2)/nrow(bcw_missing)
#missing data is biased

#Data when loaded as csv and missing values converted to NA
table(bcw2$V7)
sum(is.na(bcw2$V7))

#I started off by looking at the data and exploring what we are working with. From the summary we see that values are missing 
#in the column V7. Next, I computed the proportion of missing values in that column which came out to about 2%. This is pretty 
#low amount so we should be okay imputing the missing values. After that I created two different datasets: one with no missing 
#data points and the other solely consisting of missing values. In the missing values dataset, we see that there is a bias in the 
#data so dont use that for imputation. The clean dataset has a consistant proportion of values compared to the original. 

## Mean Method
avg1 <- mean(bcw_clean$V7) 
avg <- mean(bcw2$V7, na.rm = TRUE) #compute average excluding missing values
avg1 == avg

bcw_mean_replace <- bcw2 %>%
  mutate(V7  = ifelse(is.na(V7), avg, V7))

table(bcw_mean_replace$V7)

#I took the mean of column V7 excluding the missing values, which came out to be around 3.54. Then I mutated the column V7 with the 
#mean if that value is missing. If that column already has a value, then it is skipped over until there is a missing value cell. I printed 
#out the table so you could see the new distribution of values in the column.

## Mode Method 
the_mode <- function(x){
  uniq <- unique(x)
  uniq[which.max(tabulate(match(x, uniq)))]
}

mdv7 <- the_mode(bcw2[-impute_me,"V7"])
mdv7

bcw_mode <- bcw2
bcw_mode[impute_me,]$V7 <- mdv7

table(bcw_mode$V7)

#Mode was a little harder since there is no built in function but there are a lot of examples on Stack Overflow. You could also just look 
#at the table of the column V7 and choose it that way. We replaced the values at the index's in impute_me with the mode value, which happens 
#to be 1. The table shows 418 which is 16 higher than the original count so we have done it correctly. 

## Imputation using regression
impute <- mice(bcw2, method = 'norm.predict')
#norm.predict is linear regression, predicted values
bcw_regression <- complete(impute)

table(bcw_regression$V7)
sum(is.na(bcw_regression$V7))

#You can do this the long way by running a lm model and predicting the missing values, which I did previously. But when I was researching, 
#I found a function that does the linear regression for you and it's made for imputing missing values. All you have to do is specify the data 
#and which method to use. There is a bunch of different methods but norm.predict does linear regression and prediction. As you can see from 
#the table all the values aren't the same as the previous methods. It has a lot more range which will see later if that helps us.

## Imputation using Regression with perturbation 
impute2 <- mice(bcw2, method = 'norm.nob')
bcw_regression_perturb <- complete(impute2)

table(bcw_regression_perturb$V7)
sum(is.na(bcw_regression_perturb$V7))

#This code is very similar to the previous one but this time we choose a different method. If you look at the list of 
#methods norm.nob using linear regression with perturbation.  

## Quality of Classification Models:
# Dataset by removing the missing values
bcw_removed <- na.omit(bcw2)
table(bcw_removed$V7)
length(bcw_removed$V7)

#One of the optional questions said to run a classification model on a dataset with the missing values removed. One nice thing 
#about using the bcw2 dataset is that when it was imported it set the values "?" to NA so we can use functions that relate to NA. 
#Thus, we just omit the rows that have values of NA.

## Train with Knn
ctrl <- trainControl(method="repeatedcv", #cross validation 
                     number=10,
                     repeats = 3) # repeats the process 3 times 

#knn with missing values computed with mean
knn.mean <- train(bcw_mean_replace[,1:10],
                  as.factor(bcw_mean_replace[,11]), 
                  method = "knn", 
                  trControl = ctrl, 
                  preProcess = c("center","scale"), 
                  tuneLength = 10)

#knn with missing values computed with mode
knn.mode <- train(bcw_mode[,1:10],
                  as.factor(bcw_mode[,11]), 
                  method = "knn", 
                  trControl = ctrl, 
                  preProcess = c("center","scale"), 
                  tuneLength = 10)

#knn with missing values computed with regression
knn.reg <- train(bcw_regression[,1:10],
                 as.factor(bcw_regression[,11]), 
                 method = "knn", 
                 trControl = ctrl, 
                 preProcess = c("center","scale"), 
                 tuneLength = 10)

#knn with missing values computed using regression with perturbation
knn.reg.pert <- train(bcw_regression_perturb[,1:10],
                      as.factor(bcw_regression_perturb[,11]), 
                      method = "knn", 
                      trControl = ctrl, 
                      preProcess = c("center","scale"), 
                      tuneLength = 10)

#knn with missing values removed
knn.remove <- train(bcw_removed[,1:10],
                    as.factor(bcw_removed[,11]), 
                    method = "knn", 
                    trControl = ctrl, 
                    preProcess = c("center","scale"), 
                    tuneLength = 10)

#The ctrl part is what the train function uses to cross validate instead of having to run two different functions. 
#We are trying to predict the column V11, so we convert it into a factor since there are only two possibilities. 
#We want to center and scale the function so we can specifiy it in the function. The only thing that is different between 
#the models is the input data, but all the tuning parameters are the same.

## Plot Values 
plot(knn.mean, 
     col = 'Blue', lwd=2, cex = 3,
     main="Mean Imputed",
     xlab="# neighbors",
     ylab="Accuracy")

plot(knn.mode, 
     col = 'Blue', lwd=2, cex = 3,
     main="Mode Imputed",
     xlab="# neighbors",
     ylab="Accuracy")

plot(knn.reg, 
     col = 'Blue', lwd=2, cex = 3,
     main="Regression Imputed",
     xlab="# neighbors",
     ylab="Accuracy")

plot(knn.reg.pert, 
     col = 'Blue', lwd=2, cex = 3,
     main="Regression with Perturbation Imputed",
     xlab="# neighbors",
     ylab="Accuracy")

plot(knn.remove, 
     col = 'Blue', lwd=2, cex = 3,
     main="Removed Values",
     xlab="# neighbors",
     ylab="Accuracy")

#I plotted all the models so that you can see the accuracy of the model compared to the number of neighbors. 
#The removed value had the best prediction compared to the rest which means, or imputed values might not have been 
#the best prediction. The amount of neighbors seems to be the most accurate above 17.

## Best k value & Errors
knn.mean$bestTune
knn.reg$bestTune
knn.reg.pert$bestTune
knn.remove$bestTune

100*(1-knn.mean$results$Accuracy)
100*(1-knn.mode$results$Accuracy)
100*(1-knn.reg$results$Accuracy)
100*(1-knn.reg.pert$results$Accuracy)
100*(1-knn.remove$results$Accuracy)

#To no surprise the models have different number of neighbors that maximize the accuracy of the model. 
#The last things that are printed out are the errors in the models. You noticed that there are multiple error values 
#and that is because we cross validated the models. None of the error values was over 4% so we are pretty accurate with 
#the imputed values. Though we did not split our data, so we are definitely overfitting with the models. 
