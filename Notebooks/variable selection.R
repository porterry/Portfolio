library(tidyverse)
library(glmnet)
library(DAAG)

rm(list = ls())

#setwd("/Users/Ryan/Desktop/DS/")

#Using the crime data set from statsci I ran Stepwise Regression, LASSO, and Elastic Net before making Linear Regression models. 
#The code below explores each of the different variable selection models and reports the R^2 value for each. 

crime_data = read.table("uscrime.txt.", 
                        sep="", 
                        fill=FALSE, 
                        strip.white=TRUE,
                        header = TRUE)

s_crime_data = cbind(as.data.frame(scale(crime_data[,1])),
                     as.data.frame(crime_data[,2]),
                     as.data.frame(scale(crime_data[,c(3,4,5,6,7,8,9,10,11,12,13,14,15)])),
                     as.data.frame(crime_data[,16]))

colnames(s_crime_data) = colnames(crime_data)

#First, we clear the environment and load in the necessary libraries. Then, we set the working directory 
#in order to load the crime data from that folder. We load in the crime data and after that we make a scaled 
#copy of the data. We scale all columns except for the second one because it is a binary variable which shouldn't 
#be scaled. However, the first and last column are screwed up with the scaling, so we reset the names of the columns.


#Backward Elimination 
model_back <- lm(Crime~. , data = crime_data)
step(model_back, 
     direction = 'backward')

step(model_back, 
     direction = 'backward',
     trace = 0) #suppress the output 

#e^(diff of AICs, then divide by 2)
#how to compare to models of AIC
exp((503.93-505.16)/2) #54.06%
# about 54% chance that the model with higher AIC(505) is actually a better model than the lower model (503)

#Forward Elimination 
model_foward <- lm(Crime~1, data = crime_data)
step(model_foward,
     scope = formula(lm(Crime~., data = crime_data)),
     direction = 'forward')

## Stepwise Regression ##
model_both <- lm(Crime~., data = crime_data)
step(model_both,
     scope = list(lower = formula(lm(Crime~1, data = crime_data)),
                  upper = formula(lm(Crime~., data = crime_data))),
     direction = 'both')

#took the last predictors from the last stepwise regression model
#
final_model <- lm(Crime ~ M + Ed + Po1 + M.F + U1 + U2 + Ineq + Prob, 
                  data = crime_data)
summary(final_model)
AIC(final_model)
plot(final_model)

#Stepwise regression is a combination of forward and backward elimination, so the scope needs to include both formulas. 
#The direction is set to both since we are moving in either of the directions. I made a model using the most important 
#predictors from the stepwise results. You can see from the plots that the assumption of normality is upheld because we 
#meet all the conditions. The data follows the qq plot fairly well and the residuals are within boundry, so I'd say normally distrubted. 

cv_model <- cv.lm(crime_data, final_model, m=5)
sse <- 60677 * nrow(crime_data)  
# mean squared error 
rsq <- 1 - sse / sst
rsq

#I cross validated the model to help with overfitting and then computed the R sqaured. The results of the R^2 
#was 0.586 which was lower than I expected and previous homeworks. Maybe I filtered the model too much, 
#but it also could be the method we are using. 

## LASSO ##
#cv.glmnet() needs seed set and produces lambdas (taus in lecture video)
set.seed(42)

model_lasso <- cv.glmnet(x = as.matrix(crime_data[,-16]),
                         y = as.matrix(crime_data[,16]),
                         alpha=1, # 1 = Lasso method
                         nfolds = 8,
                         nlambda = 20,
                         type.measure = 'mse',
                         family = 'gaussian',
                         standardize = TRUE) #scales the data

plot(model_lasso)
model_lasso$lambda.min #8.839
cbind(model_lasso$lambda, model_lasso$cvm, model_lasso$nzero) #s7 has the smallest error and uses 11 predictors
coef(model_lasso, s=model_lasso$lambda.min)

#We need to set the seed because the cv.glmnet function uses some randomization, so it lets us replicate the results. 
#In the function alpha needs to set to 1 because that's the value for running LASSO method. If alpha was set to 0 then 
#the function would run as Ridge Regression. The cbind function combines the output of our Lasso function, giving us 
#the lambda values with MSE and predictors. The coef function shows the predictors with their minmum value for Lambda, 
#so any zero values are not important.

sse2 <- 62392.77 * nrow(crime_data)  
## total sum of squares
sst <- sum((crime_data$Crime - mean(crime_data$Crime))^2)
# mean squared error 
rsq2 <- 1 - sse2 / sst
rsq2 #.57

#Next, we compute the R squared value for the Lasso model, which came out to be 0.57. About the same value as the 
#Stepwise model so maybe our previous homeworks were overfitting since they had higher R^2 values. This is just 
#a guess about why my values this week are lower than previous.

final_lasso <- lm(Crime ~ M + So+ Ed + Po1 + M.F + NW + U1 + U2 + Wealth + Ineq + Prob, 
                  data = s_crime_data)

cv_model <- cv.lm(s_crime_data, final_lasso, m=5)
sse2 <- 63086 * nrow(crime_data)  
# mean squared error 
rsq2 <- 1 - sse2 / sst
rsq2

#I decided to run a model where I took out the least important factors according to LASSO and run the linear model again. 
#This time I got a lower value by about 0.02, which means I could have taken out a predicting factor or more than likely it's 
#random error. The more I tried to optimize the model the worse the R squared value becomes so this value will suffice. 

## Elastic Net ## 
#Test different values of Alpha between 0-1
#pick best alpha value based on R^2 or MSE

r2=c()
for (i in 0:100) {
  model.elastic = cv.glmnet(x=as.matrix(crime_data[,-16]),
                            y=as.matrix(crime_data[,16]),
                            alpha=i/100,
                            nfolds = 5,
                            nlambda = 20,
                            type.measure="mse",
                            family="gaussian",
                            standardize = TRUE)
  
  #dev.ratio is the percentage of deviance explained
  #min index for the dev.ratio of the model
  m = which(model.elastic$glmnet.fit$lambda == model.elastic$lambda.min)
  r2 = cbind(r2, model.elastic$glmnet.fit$dev.ratio[m])
}
r2

alpha_best = (which.max(r2)-1)/100
alpha_best

#The last model that we needed to run was the Elastic Net model which is shown above. This model 
#format is very close to LASSO except that we are concerned with the Alpha value. The for loop goes 
#through 101 different values for alpha and computes the R sqaured value. I found the R squared computation 
#online because I didn't undertand how to calculate it from the results. However, dev.ratio shows the % of deviance 
#explained, which in the context of regression is equal to R squared. To get the best Alpha we want the one with 
#the lowest R squared which we'll use to run the model again. 

model_enet <- cv.glmnet(x = as.matrix(crime_data[,-16]),
                         y = as.matrix(crime_data[,16]),
                         alpha = alpha_best, 
                         nfolds = 8,
                         type.measure = 'mse',
                         family = 'gaussian',
                         standardize = TRUE) #scales the data

model_enet$lambda.min #6.895
cbind(model_enet$lambda, model_enet$cvm, model_enet$nzero) #s45 has smallest error
coef(model_enet, s=model_enet$lambda.min)

final_enet <- lm(Crime ~ M + Ed + Po1 + U2 + Ineq + Prob, 
                  data = s_crime_data)

cv_model <- cv.lm(s_crime_data, final_enet, m=5)
sse3 <- 53586 * nrow(crime_data)  
# mean squared error 
rsq3 <- 1 - sse3 / sst
rsq3

#We plug in the Alpha with the lowest R squared back into the equation and run it one last time. 
#Then we are just repeating the steps again and computing the R sqaured to see how it compares. 
#Elastic Net gives us the best R squared of the three models at 0.634, which is a big improvement compared 
#to the other two. In my limited experience I have noticed that Elastic Net in general performs consistently 
#better than most models. It would be interesting to combine this method with PCA.
