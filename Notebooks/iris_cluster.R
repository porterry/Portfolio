#Use the R function kmeans to cluster the points as well as possible. 
#Report the best combination of predictors, your suggested value of k, and how well your best clustering predicts flower type.

data(iris)  #load the iris data

summary(iris) #a quick look at the data

set.seed(0222)

#must load tidyverse in order to use %>%
iris %>% ggplot(aes(Petal.Length, Petal.Width, color = Species)) + 
  geom_point()

iris %>% ggplot(aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_point()

iris %>%
  group_by(Species) %>%
  count()

#The first graph is of petal length by the petal width and I have it color cordinated by the species of flower. 
#The second graph is very similiar to the first but instead of petal we are using the sepal lenth and width. 
#Last, is a simple table of the number of each species of flower which is 50 for each of them.

#cluster <- kmeans(iris[, 2:4], 3, nstart = 20) 
#predicts virginica less accurately by one flower each time

#testing out different numbers of clusters. My guess is going to be that 3 is optimal 
#because we are talking about three different species of flower. 
cluster_2 <- kmeans(iris[, 3:4], 2, nstart = 10)
cluster_3 <- kmeans(iris[, 3:4], 3, nstart = 10)
cluster_4 <- kmeans(iris[, 3:4], 4, nstart = 10)
cluster_5 <- kmeans(iris[, 3:4], 5, nstart = 10)
table(cluster_2$cluster, iris$Species)
table(cluster_3$cluster, iris$Species)
table(cluster_4$cluster, iris$Species)
table(cluster_5$cluster, iris$Species)

#You can see from each of the tables that k values bigger than 3 seem to start miss cateogizing them a lot more. 
#However, less than 3 you are trying to fit two different groups of species into one group which doesnt cluster than correctly. 
#I decided to go with k=3. I tried different values of nstart (5,20) but didnt seem to get much of a variation when changing it. 
cluster1 <- kmeans(iris[, 3:4], 3, nstart = 10) 
# best prediction from the models
cluster1

cluster2 <- kmeans(iris[, 1:2], 3, nstart = 10)

table(cluster1$cluster, iris$Species)
table(cluster2$cluster, iris$Species)

#The best combination of predictors was petal length and petal width (cluster1). 
#This makes sense because each petal is very specific to the flower where sepal length is not as specific to each of them. 
#My model predicted all 50 setosa correctly, 38 of 50 correct for versicolor, and 35 of 50 correct for virginica. 
#From the table you can see that versicolor and virginica seem to miss clustered for each other but never setosa. 