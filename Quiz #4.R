getwd()
setwd('/Users/zacklarsen/Desktop/CSC 529/Data')
getwd()

ForestCover2500 <- read.csv('ForestCover2500.csv',sep = ',')

#How many observations does the dataset have?
length(ForestCover2500$X)

#Let us create a training and testing set as we have done before:
set.seed(1234)
ind <- sample(2, nrow(ForestCover2500), replace=TRUE, prob=c(0.8, 0.2))
train <- ForestCover2500[ind==1,]
test <- ForestCover2500[ind==2,]

#How many observations fall in the training set?
length(train$X)




#Next, let us create a simple decision tree as we have done before:
library(rpart)
tree.a <- rpart(Cover ~ . , data=train)
plot(tree.a)
text(tree.a)

#What was the most informative feature?
#Elevation


# 24 As before, we can use caret to evaluate the model:
#install.packages("pbkrtest")
#install.packages("caret")
library(caret)
pred.a <- predict(tree.a, test, type = "class")
t <- table(pred.a,test$Cover)
confusionMatrix(t)

#What is the accuracy of the model?
# 0.6388




# 25 Recall that a random forest is a collection of trees.  Each tree differs
#from the others, in part, by the data used to train it.  We can create a 
#bootstrapped partition using “sample”.  Try this code:
set.seed(1234)
sample(10)
sample(10, replace=T)

#In the first example, 10 random numbers 1-10 were selected at random.
#However, each number was selected only once.  In the second example, sampling 
#with replacement was performed.  How many times was 3 selected?
# 3 was selected 5 times



# 26 We will use “sample” to create our bootstrapped dataset:
set.seed(1234)
b <- sample(nrow(train), replace=T)

#Inspect ‘b’ and use “length” to find its length.
length(b)



# 27  The “unique” command will return a vector of unique elements.  For example…
u <- unique(b)
#…will remove the duplicate elements from b.
#What is the length of u?
length(u)
#8875


#  28 What percent of the data made it into the bootstrapped sample? 
#Give your answer as a decimal.
bootstrapped_sample = (length(u)/nrow(train))
bootstrapped_sample
# 0.6345178


# 29 Now we will create our training data for the first decision tree.  
#Further, this tree will use only the first 4 features.
b.train.1 <- train[b,c(2:5,16)]
#Then we can produce a decision tree…
tree.1 <- rpart(Cover ~ . , data=b.train.1)
pred.1 <- predict(tree.1, test, type = "class")
#…and compute its accuracy.
t <- table(pred.1,test$Cover)
confusionMatrix(t)

#What is the accuracy of this tree?
# 0.6095







# 30 Now for another tree, trained on another bootstrap sample and using only
#features 5 through 8.
b <- sample(nrow(train), replace=T)
b.train.2 <- train[b,c(6:9,16)]
tree.2 <- rpart(Cover ~ . , data=b.train.2)
pred.2 <- predict(tree.2, test, type = "class")

t <- table(pred.2,test$Cover)
confusionMatrix(t)

#What is the accuracy of the second tree?
# 0.3646







# 31 We will now build one last tree, built on yet another bootstrap sample 
#and this time on features 9 through 14.
b <- sample(nrow(train), replace=T)
b.train.3 <- train[b,c(10:15,16)]
tree.3 <- rpart(Cover ~ . , data=b.train.3)
pred.3 <- predict(tree.3, test, type = "class")
t <- table(pred.3,test$Cover)
confusionMatrix(t)

#What is its accuracy?
#0.3769







#Now let us combine the results.  We will reuse our three trees, now 
#outputting their probabilities rather than class predictions:
pred.1.p <- predict(tree.1, test)
pred.2.p <- predict(tree.2, test)
pred.3.p <- predict(tree.3, test)

#With what probability did the second tree predict “Krummholz" for the 
#first test observation?  You will need to peek into the dataframe.
# 0.291922290
#0.35657075



#R makes adding matrixes together easy:
predictions <- (pred.1.p + pred.2.p + pred.3.p)/3
#What was the average prediction for “Krummholz” on the first testing example?
# 0.23546707
#0.24834186




#We can use the average probabilities to produce a new set of class predictions:
col <- apply(predictions,1,which.max)
labels <- colnames(predictions)
p <- labels[col]
t <- table(p,test$Cover)
confusionMatrix(t)
#What is the prediction accuracy when all three trees are combined?
#0.6556 
#0.6359


#By how much did our (Very!) crude random forest improve our accuracy
#over the baseline decision tree we evaluated?
0.6359-0.6095
#0.0264









#Of course, in practice we would never build a decision tree this way.
#We will turn our attention to “randomForest,” an R package.  
#Download and install it, if do not have it already:
#install.packages("randomForest")
library(randomForest)
help(randomForest)

#Read the documentation.  What argument controls whether or not the algorithm
#samples with replacement?
# replace	Should sampling of cases be done with or without replacement?


#Let us create a training and testing set as we have done before:
set.seed(1234)
ind <- sample(2, nrow(ForestCover2500), replace=TRUE, prob=c(0.8, 0.2))
train <- ForestCover2500[ind==1,]
test <- ForestCover2500[ind==2,]



#We will first use “randomForest” to build three decision trees.
rf.3 <- randomForest(Cover ~ ., data=train, ntree=3)
pred.rf.3 <- predict(rf.3, test, type = "class")
t <- table(pred.rf.3,test$Cover)
confusionMatrix(t)

#What is the accuracy?
# 0.7874  
# 0.7723 


#Build a random forest with 50 trees.  What is the accuracy?  
#Remember to reset your random seed.
set.seed(1234)
rf.50 <- randomForest(Cover ~ ., data=train, ntree=50)
pred.rf.50 <- predict(rf.50, test, type = "class")
t <- table(pred.rf.50,test$Cover)
confusionMatrix(t)
# Accuracy = 0.8679 



#Now try 500 trees!
set.seed(1234)
rf.500 <- randomForest(Cover ~ ., data=train, ntree=500)
pred.rf.500 <- predict(rf.500, test, type = "class")
t <- table(pred.rf.500,test$Cover)
confusionMatrix(t)
# Accuracy = 0.8773 


#Experiment with different numbers of trees in the random forest.

# Random forest with 10 trees.  
set.seed(1234)
rf.100 <- randomForest(Cover ~ ., data=train, ntree=10)
pred.rf.100 <- predict(rf.100, test, type = "class")
t <- table(pred.rf.100,test$Cover)
confusionMatrix(t)
# Accuracy = 0.8346  


# Random forest with 20 trees.  
set.seed(1234)
rf.100 <- randomForest(Cover ~ ., data=train, ntree=20)
pred.rf.100 <- predict(rf.100, test, type = "class")
t <- table(pred.rf.100,test$Cover)
confusionMatrix(t)
# Accuracy = 0.8545  



# Random forest with 30 trees.  
set.seed(1234)
rf.100 <- randomForest(Cover ~ ., data=train, ntree=30)
pred.rf.100 <- predict(rf.100, test, type = "class")
t <- table(pred.rf.100,test$Cover)
confusionMatrix(t)
# Accuracy = 0.8591    


# Random forest with 60 trees.  
set.seed(1234)
rf.100 <- randomForest(Cover ~ ., data=train, ntree=60)
pred.rf.100 <- predict(rf.100, test, type = "class")
t <- table(pred.rf.100,test$Cover)
confusionMatrix(t)
# Accuracy = 0.8659 


# Random forest with 80 trees.  
set.seed(1234)
rf.100 <- randomForest(Cover ~ ., data=train, ntree=80)
pred.rf.100 <- predict(rf.100, test, type = "class")
t <- table(pred.rf.100,test$Cover)
confusionMatrix(t)
#Accuracy = 0.8713  
.8713-0.8679


# Random forest with 100 trees.  
set.seed(1234)
rf.100 <- randomForest(Cover ~ ., data=train, ntree=100)
pred.rf.100 <- predict(rf.100, test, type = "class")
t <- table(pred.rf.100,test$Cover)
confusionMatrix(t)
#Accuracy = 0.8722


# Random forest with 250 trees.  
set.seed(1234)
rf.250 <- randomForest(Cover ~ ., data=train, ntree=250)
pred.rf.250 <- predict(rf.250, test, type = "class")
t <- table(pred.rf.250,test$Cover)
confusionMatrix(t)
#Accuracy = 0.8759  


































































