## Load Data
#Using the 20 years of daily high temperature data for Atlanta, build and use an exponential smoothing model 
#to help make a judgment of whether the unofficial end of summer has gotten later over the 20 years. 

library(tidyverse)
library(stats)

temp_data = read.table("temps.txt.", 
                       sep="", 
                       fill=FALSE, 
                       strip.white=TRUE,
                       header = TRUE)

head(temp_data)
summary(temp_data)

#Just loading the data and the necessary packages. Also, the summary and head just give an idea of the data we are working with. 
#Though we should be famailiar with the data because this is from last week, but never hurts to explore.

#Plot the time series
temp_ts<-ts(as.vector(unlist(temp_data[,2:21])),start=1996,frequency=123)
summary(temp_ts)
#plot the time series
ts.plot(temp_ts)

#Need to convert the data into a time series data which is done with the ts function.
#It needs to be in a vector or a matrix so I decide to convert it into a vector. 
#We see that the mean temperture of the data is 83.34 with a max of 105 and min of 50. 
#The time series plot helps visualize the data and from just looking at the data there 
#is a lot of flucation of temperature during theses months. 

# Exponential Smoothing
temp_holt <- HoltWinters(temp_ts, seasonal = "additive")
temp_holt_ml <- HoltWinters(temp_ts, seasonal = "multiplicative")
summary(temp_holt)
summary(temp_holt_ml)
temp_holt$SSE
temp_holt_ml$SSE

plot(fitted(temp_holt))
plot(fitted(temp_holt_ml))

head(temp_holt$fitted)

#There are two different approaches to the Holtwinters function additive and multiplicative which
#compute the four components differently. The additive sums up the four compenents and the multiplicative 
#uses the product of the four. We can see that additive has a smaller sum of the squared errors so we will 
#use that for our model. If we look at the fitted model for temp_holt we see that there isnt much of a trend. 
#The same is true for the multiplicative model so from the surface it is harder to tell if summers are getting hotter. 
#But we can now use our computed fitted model values and use cusum to try and detect an increase in temperature.  

#create matrix to store season values
season <- matrix(temp_holt_ml$fitted[,4],nrow=123)

#write.csv(season, file="season.csv", row.names = F)

colnames(season) <- colnames(temp_data[,3:21])
rownames(season) <- temp_data[,1]

#I created a matrix to hold the season values since we are interested in running those values in our cusum function. 
#I wrote the values and explored them in Excel which lead to similiar findings. Then I add the row names and colnames
#to the matrix so it is easier to navigate the matrix.  

#avg of all the years
avg_allyrs <- mean(season)
avg_allyrs

#a look at an average from dates we 
#know that are fall time 
which(temp_data$DAY=="1-Oct")
mean(season[93:123,])

#Avg sf for the 1st year
##use this as the baseline to mark end of summer
avg_year1 <- mean(season[,1])
avg_year1

#I first take at the average for all the years which we see is almost one. Since we 
#are interested in if the end of summer has gotten later then lets look at a fall day's average. 
#October first is a fall day and the average on that day across the years is 0.87 which is about 
#0.12 less than the average. We need to determine a baseline of when summer ends for the cusum function 
#so lets take the seasonal factor of the first year, which is 1. 

#Cusum
cusum_fn = function(data, avg, T, C){
  #an empty list to hold results
  results = list()
  cusum = 0 #intial 0
  Counter = 1 #a counter 
  while (Counter <= nrow(data)){
    current = data[Counter,]
    #cusum equation 
    cusum = max(0, cusum + (avg - current - C))
    if (cusum >= T) {
      results = Counter
      break
    }
    Counter = Counter + 1
    if (Counter >= nrow(data)){
      results = NA
      break
    }
  }
  return(results)
}

# C is half the std the 1st yr
# Threshold is 3 time the std 
C_val = sd(season[,1])*0.5
Thres = sd(season[,1])*2

# Run for each year 
#see if SF was higher than the threshold
# avg of first year
result_vector = vector()
for (x in 1:ncol(season)){
  result_vector[x] = cusum_fn(data = as.matrix(season[,x]), 
                              avg =  avg_year1, 
                              T = Thres, 
                              C = C_val)
}

#store the results in a dataframe
results = data.frame(Year = colnames(season), 
                     Day = temp_data[result_vector,1])
results

#Finding a good C value and T value was difficult and I did a lot of trial and error. 
#I ran it with threshold value multipliers of 3, 4, and 5 but they all produced similiar results. 
#Where the end of summer was slightly getting later even if you marked the end of summer later. 
#I also changed values of C from about 0.2 to 1 but again it produced the same results but it errors detecting dates. 
#So I decided that the multipliers for C would be 0.5 and T would be 2. The results for loop apply the C and 
#T values to the cusum function and then is printed out in a data frame. As you can see from the results the day 
#is slowly getting later into October. This is indicating that the average temperature is rising meaning that global warming is happening. 

#Predict
predicts <- predict(temp_holt, 200, prediction.interval = TRUE)
plot(temp_holt, predicts)

#I tried to predict out just to see what the future might look like but the confidence interval is very large. 
#I did run it through the cusum model but I did not have it set up correctly because it was only predicting one year. 
#Though from the model it looks like temperature could trend down, but the interval is so large that it's not conclusive.


