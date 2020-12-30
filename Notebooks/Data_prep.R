# Clear environment
rm(list = ls())


library(tidyverse)
library(DAAG)
library(boot)
library(GGally)

#setwd("/Users/Ryan/Desktop/DS")

crime_data = read.table("uscrime.txt.", 
                        sep="", 
                        fill=FALSE, 
                        strip.white=TRUE,
                        header = TRUE)

#test data 
crime_test <- data.frame(M = 14.0, So = 0, 
                         Ed = 10.0, Po1 = 12.0, 
                         Po2 = 15.5, LF = 0.640, 
                         M.F = 94.0, Pop = 150, 
                         NW = 1.1, U1 = 0.120,
                         U2 = 3.6, Wealth = 3200, 
                         Ineq = 20.1, Prob = 0.04, 
                         Time = 39.0)

#Loading in the four packages that will be used throughout the problem. Next is setting the working directory 
#and reading in the crime data that was give to us. The crime test data is the information about the city which we 
#are trying to predict the crime rate for with PCA. Once we build the model, we are trying to predict the crime rate 
#given those values about the city and then see how well our model does compared to the cross validated model.

## Cross Validation ##
lm_model <- lm(Crime ~ M + Ed + Po1 + U2 + Ineq + Prob,
               data = crime_data)

summary(lm_model)

#cross validate
cv_model <- cv.lm(crime_data, lm_model, m=5)
# We can calculate the R-squared values directly.
# R-squared = 1 - SSEresiduals/SSEtotal
# total sum of squared differences between data and its mean
sse <- 48203 * nrow(crime_data)  
## total sum of squares
sst <- sum((crime_data$Crime - mean(crime_data$Crime))^2)
# mean squared error 
rsq <- 1 - sse / sst
rsq

predict1 <- predict(lm_model, crime_test)
predict1 #1304
AIC(lm_model)

#The R-squared for the model is 0.671. This was the best performing model that I made from last week. 
#We are still potentially overfitting with this model though so it'll be interesting to see the new model. 
#The second test of fit is the AIC value which models with lower values are more accurate. Again, this model 
#had the lowest value for AIC so I would say that it is the best of the lm models for predicting crime rate. 

## PCA ##
#Using the same crime data set as the previous question, I will apply Principal Component Analysis and then create a 
#regression model using the first few principal components.  Then after that I will compare the quality of the PCA 
#model with the cross validate model.

#Correlation in the data 
ggpairs(crime_data, columns = c('Po1', 'Po2', 'U1', 'Ineq'))

pca_model <- prcomp(crime_data[,1:15], scale = TRUE)
summary(pca_model)
#a lot of variance is from the first 5 predictors 

#plot the variances of each of the principal component
screeplot(pca_model, type = 'lines', col = 'blue')

#The GGpairs function was shown during the Monday lecture and so I used it also to look at the correlation between 
#the predictors. From the graph it is clear that Po1 and Po2 have a strong correlation between the two. Ineq had a 
#strong correlation with Wealth, Po1, and Po2 so that could be problematic. Lets run the pca function on the dataset 
#but make sure you don't include crime data and scale it. Screeplot was shown also during the Monday lecture, which it 
#plots the variances of each of the principal components. From the graph it is obvious that first principal component has 
#the biggest varinance and then pc 2, pc3, pc4, ... So, from my model I am going to choose the first 5 predictors since they 
#account for the majority of the variance. 

#obtain the 5 principal components from result matrix
#since they composed of the most variance 
principal_comp <- pca_model$x[,1:5]
# now create a new matrix with components and crime response
pca_matrix <- cbind(principal_comp, crime_data[,16]) 

#lm model using first 5 pc
pca_lm_model <- lm(V6 ~., 
                   data = as.data.frame(pca_matrix)) 
summary(pca_lm_model)

#First let's make a matrix of the first five principal components that we will use to make our model. 
#In order to make our model we also need the crime rate data so let's combine our matrix with that column. 
#Then we can run the Linear Regression model on the pca which will hopefully make a more accurate prediction that 
#the LM model from last week. Looking at the summary, we see that pca model has a lower R^2 but let's compute a 
#more accurate R^2 that isnt scaled. 

k = 5

#beta zero or the intercept
intercept <- pca_lm_model$coefficients[1]
intercept

#betas; slopes from scaled PCA regression
betas <- pca_lm_model$coefficients[2:(1+k)]
betas

#pca_model$roatation is the matrix of eigenvectors
#a_j=b_k*v_jk
#b:coefficients
#v: rotation matrix
#j: original factors
#k: principal components
# %*% matrix multiplication
alpha <- pca_model$rotation[,1:k]%*%betas

#unscale alpha by dividng by the scale
alpha_unscaled <- alpha/pca_model$scale 
alpha_unscaled

beta0_unscaled <- intercept - sum(alpha*pca_model$center/pca_model$scale)#unscaled intercept
beta0_unscaled

#model y = ax + b
y <-  as.matrix(crime_data[,1:15])%*%alpha_unscaled + beta0_unscaled 
#Calculate the R^2 error using the equations from last week
rss2 <- sum((y - crime_data[,16]) ^ 2)  ## residual sum of squares
rsq2 <- 1 - rss2/sst
rsq2 # R-squared of PCA

AIC(pca_lm_model)

#All of this above code relates to unscaling the data and computing the R^2 of the pca model. The equation 
#a_j=b_k*v_jk is used to compute the aplha. We need to take the eigenvectors and times by the beta to get our alpha. 
#Then we want to unscale the alpha so we can use it to compute the R^2. We have to unscale the beta in order to compute 
#our model y=mx+b. Once we have the unscaled and calculate the y then we can compute the R^2. Our R^2 is 0.645 which is 
#lower than our previous 0.671 but cross validated model could have overfitted the data. Our AIC score is worse than CV 
#model at 658 and previously it was 640. So, our lm model using PCA did predict worse than CV model. 

#Predict crime rate with pca model
pred_df <- data.frame(predict(pca_model, crime_test)) 
predict2 <- predict(pca_lm_model, pred_df)
predict2

#Finally, we are able to predict our crime rate which resulted in 1389. Last week we got 1304 so a little bit lower than 
#our PCA model. It looks like scaling the data and using the principle component analysis might have gave us a more accurate 
#predicion than standard lm. Even though our accuracy is lower it seems that we are not overfitting the data like CV model. 
#This test was on a pretty small sample size so it would be interesting to see the results of the scaling with a large data set.
