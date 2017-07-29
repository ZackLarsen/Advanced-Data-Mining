# CSC 529 Quix #5 Ensemble clustering
getwd()
setwd('/Users/zacklarsen/Desktop/CSC 529/Data')
getwd()

#In this tutorial, we will explore ensemble clustering in R.  
#Download the cities10k dataset from the D2L and load it into R.  
cities10k <- read.csv('cities10k.csv',sep = ',')
# 17 According to the dataset what is the population of Shanghai?
#14608512

# 18 We will start by extracting just the longitude and latitude from the data:
loc <- cities10k[,6:7]
#When you plot the latitude and longitude of the cities, you should see a rough sketch of the continents. “pch” changes the marker type and “cex” changes the marker size:
plot(loc$Longitude,loc$Latitude,  pch = 20, cex = .1)
#According to the scatterplot, which side of Australia is most heavily populated?
# The east side



# 19 Let us now create a k-means partitioning and plot the points again, coloring the points by their assigned cluster:
set.seed(1234)
clust.k02 <- kmeans(loc, 2)
plot(loc$Longitude,loc$Latitude, pch = 20, cex = .1, col = clust.k02$cluster)
#What color is the cluster of Southern African cities?
# Red



# 20 Let us try it again with five clusters:
set.seed(1234)
clust.k05 <- kmeans(loc, 5)
plot(loc$Longitude,loc$Latitude,  pch = 20, cex = .1, col = clust.k05$cluster)
#What color is the cluster of Southern African cities?
# Blue





# 21 And now with 10 clusters:
set.seed(1234)
clust.k10 <- kmeans(loc, 10)
plot(loc$Longitude,loc$Latitude, pch = 20, cex = .1, col = clust.k10$cluster)
#What color is the cluster of Southern African cities?
# Green



# 22 … and 25 clusters
set.seed(1234)
clust.k25.1 <- kmeans(loc, 25)
plot(loc$Longitude,loc$Latitude,  pch = 20, cex = .1, col = clust.k25.1$cluster)
#What color is the cluster of Southern African cities?
# Blue


# 23 Try k-means clustering again (k=25) again, but with a different random seed:
set.seed(2345)
clust.k25.2 <- kmeans(loc, 25)
plot(loc$Longitude,loc$Latitude,  pch = 20, cex = .1, col = clust.k25.2$cluster)
#What color is the cluster of Southern African cities?
# Red


# 24 Once more with yet a different seed:
set.seed(3456)
clust.k25.3 <- kmeans(loc, 25)
plot(loc$Longitude,loc$Latitude,  pch = 20, cex = .1, col = clust.k25.3$cluster)
#What color is the cluster of Southern African cities?
# Purple



# 27 We will now employ “clue” to create an ensemble of clusters.  
#Download and install “clue”:
#install.packages("clue")
library(clue)
#We will use the “cl_boot” function to generate several partitioning.  
#What is the default clustering approach?
help(cl_boot)
# Default clustering approach is kmeans



# 28 We can create the partitions thus:
set.seed(1234)
ensemble <- cl_boot(loc, 50, 25)
ensemble
#How many partitionings are there?
# 50
#How many objects are there?
# 9999


# 30 We can look at the agreement between partitionings to 
#get an idea of their agreement:
cl_agreement(ensemble)
#Notice that the diagonal entries are 1’s.  What is the agreement between 
#the first and fourth partitioning?
# 0.4685602



# 31 We can aggregate the results of all 50 partitionings into a single result:
consensus.se <- cl_consensus(ensemble, method = "SE", control = list(k = 25))
help(cl_consensus)
#What does the “method” flag do?
# It selects the consensus method


# 32 What is the assignment prediction for object 1 to cluster 4?
consensus.se
# 0.98



# 33 We can also create a hard consensus:
consensus.he <- cl_consensus(ensemble, method = "HE", control = list(k = 25))
#What is the new assignment prediction for object 1 to cluster 4?
# 0.98



# 34 Finally, we can produce a new set of assignments and plot them at before:
assignments <- cl_class_ids(consensus.he)
plot(loc$Longitude,loc$Latitude,  pch = 20, cex = .1, col = assignments)
#What color is the cluster of Southern African cities?
# Red

# 35 Rerun the clustering ensemble several times using different
#starting seeds.  True or False: the resulting clusters appear more stable.






# 36 Is it reasonable to create an ensemble of several partitionings with different values of k?
#Post your answer on the D2L and mark True.






























