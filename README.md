# Data Analyst/Science Portfolio by Ryan Porter
This portfolio is a compilation of code projects for data analysis or machine learning algorithms.

## Projects
### Boise Zoo Project 
  While doing my certificate in Data Science from Boise State University, we did a community service project with the local zoo. Zoo Boise was interested to see if their new wildlife exhibits led children to express more empathy for the animals. This study examined children’s empathetic responses to two types of exhibits at Zoo Boise: preexisting exhibits with older interpretive features and new exhibits with updated interpretive features. Behaviors were defined and categorized based on a pre-established protocol and standardized observation instrument called the observational framework of empathetic behavior. The most challengeing part is how to convert how someone feels to data that analyzable. Zoo Boise did have a [observation sheet](https://github.com/porterry/Portfolio/blob/main/Notebooks/Empathy%20Observation.pdf) that the observers would fill out for each of the children to help keep tracking consistent. 

[PDF](https://github.com/porterry/Portfolio/blob/main/Notebooks/Zoo%20Project.pdf)

### San Francisco Bike Share 
  This was a final project for onr of my data science classes at Boise State University. The goal of this project is to create a model that can predict the amount of bike rides per day. I have three different datasets one with the trips, another is the stations that the bike was taken and returned to, and the final dataset is the weather for that day. 

[PDF](https://github.com/porterry/Portfolio/blob/main/Notebooks/Bike_Share_MD.pdf)
  
### PELT - Undergrad Research
  One of the biggest challenges in time series and sequence data is the ability to detect
multiple points of changes within that data set. Change point analysis is the process of
detecting multiple points of changes throughout time series or sequence data. There are
many different change point search methods but for this paper the focus will be on PELT
and Binary Segmentation methods. The different search methods and demonstrations of
their application with simulated and real life data sets will be explored throughout the
paper. 

[PDF](https://github.com/porterry/Portfolio/blob/main/Notebooks/Change_Point_Porter.pdf)

## Regression 
### Crime Rate Predication
Using the crime data from [statsci](http://www.statsci.org/data/general/uscrime.html), I used linear regression to predict the crime rate in a city with variables defined in the code. The dataset has few data points so overfitting provides another issue to deal with in the problem. 

[RMD](https://github.com/porterry/Portfolio/blob/main/Notebooks/crime_rate.Rmd) [PDF](https://github.com/porterry/Portfolio/blob/main/Notebooks/crime_rate.pdf)

### Regression Tree and Random Forest
The same crime dataset from the notebook above is used but instead of linear regression to predict crime rate, I use Random Forests and Regression Trees for the prediction. This dataset is used in a couple of my notebooks because it's an clean dataset that is easily manipulatable for all different types of modeling. 

[R](https://github.com/porterry/Portfolio/blob/main/Notebooks/random_forest.R) [PDF](https://github.com/porterry/Portfolio/blob/main/Notebooks/randfom_forest.pdf)

## Classification
### Titanic - Machine Learning from Disaster 
Titanic - Machine Learning from Disaster is a knowledge competition on [Kaggle](https://www.kaggle.com/c/titanic). It is a binary classification problem: based on information given about Titanic passengers we predict if they survived or not. The dataset provides interesting opportunities for feature engineering. This is a pretty common introduction machine learning problem so I thought it was a good problem to solve.

[RMD](https://github.com/porterry/titanic/blob/master/titanic_markdown.Rmd) [PDF](https://github.com/porterry/Portfolio/blob/main/Notebooks/titanic_MD.pdf)

### Credit Card 
The dataset used in this problem is credit card applications with a binary response indicating if the applicant was accepted or rejected. Dataset is from UCI Machine Learning Repository but missing values were excluded from the set that was analyzed. The goal of the project is to find the best classifier using cross validation and using testing & training datasets. 

[R](https://github.com/porterry/Portfolio/blob/main/Notebooks/credit_card_code.R) [PDF](https://github.com/porterry/Portfolio/blob/main/Notebooks/credit-card.pdf)

### Logistic Regression - Credit Risk
Another credit card dataset but this one is composed of German applicants. The goal of this notebook is to figure out if the applicant is a good credit risk or a bad credit risk. We are computing the probability that the applicant will be a good risk or not.

[R](https://github.com/porterry/Portfolio/blob/main/Notebooks/german_credit.R) [PDF](https://github.com/porterry/Portfolio/blob/main/Notebooks/german_credit.pdf)

## Clustering
### IRIS Clustering
A classic example of clustering based on the attributes of the flower. Clustering is an approach to unsupervised machine learning which will be demonstrated in the notebook. There are three different types of iris's, so the goal is to accurately cluster the flowers into the correct iris type. K-means clustering is grouping data points in clusters that have similar attributes as the other data points in the cluster. 

[R](https://github.com/porterry/Portfolio/blob/main/Notebooks/iris_cluster.R) [PDF](https://github.com/porterry/Portfolio/blob/main/Notebooks/clustering.pdf)

## Time Series
### Daily Temperature Atlanta
The goal of this project is to figure out if the end of summer has gotten later in the past 20 years. I used Holt-Winters Exponential Smoothing and Cusum to determine if the end of the summer has gotten later. Time series is a bit different to analyze since in this case it was continuous so you need to figure out where in time the change was. This problem focuses on using the daily temperature data to see if the last day of summer has gradually gotten later as the years have gone by.

[R](https://github.com/porterry/Portfolio/blob/main/Notebooks/time_series_code.R) [PDF](https://github.com/porterry/Portfolio/blob/main/Notebooks/time_series.pdf)

## Variable Selection
### Stepwise Regression, LASSO, Elastic Net on Crime Data
Using the same crime dataset from the statsci, I run multiple variable selection methods before using Linear Regression for the prediction. Each of the models also have their R <sup>2</sup> calculated in order to compare the quality of each model. This notebook also shows that some models require the data to be scaled before running the models.

[R](https://github.com/porterry/Portfolio/blob/main/Notebooks/variable%20selection.R) [PDF](https://github.com/porterry/Portfolio/blob/main/Notebooks/variable_selection.pdf)

## Data Cleaning & Prep
### Data Imputation
A lot of the datasets used previously have been in pretty good condition. What I mean by that is they are not missing values, no values in the wrong spot, and no obvious outliers. This problem focuses on what to do when missing values in the dataset. You have two options when it comes to missing values: either you can ignore the missing values or impute the missing value. Four different type of imputation are compared, in addition to ignoring the missing value. 

[R](https://github.com/porterry/Portfolio/blob/main/Notebooks/missing_data.R) [PDF](https://github.com/porterry/Portfolio/blob/main/Notebooks/missing_data.pdf)

### PCA
Again the crime rate dataset is used with regression. However, since  the dataset is so large Principal Component Analysis will be used before running the regression model. Big dataset usually have information that isn't important to the analysis so PCA reduces the dimensionality of such dataset while reducing important information loss.

[R](https://github.com/porterry/Portfolio/blob/main/Notebooks/Data_prep.R) [PDF](https://github.com/porterry/Portfolio/blob/main/Notebooks/Data_prep.pdf)



