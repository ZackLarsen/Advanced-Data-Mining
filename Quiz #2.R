# CSC 594 Quiz #1 Zack Larsen

#Install the data from the textbook 'An Introduction to Statistical Learning
#with Applications in R':
#install.packages("ISLR")
library(ISLR)
attach(Carseats)
data <- Carseats
help(Carseats)

#We can use the “head” command to get a look at the data:
head(data)
#The “range” command will display the min and max of a variable:
range(data$Sales)
#We can look at a report of all the variables using the “summary” command:
summary(data)

#create a new variable called “StrongSales”:
data$StrongSales <- as.factor( ifelse(data$Sales >=10, "Yes", "No") )

#Notice a few things:
#“ifelse” tests the condition (in this case, “data$Sales >=10”), and if 
#true outputs the second argument (“yes”).  Otherwise it outputs the third 
#argument (“no”).
#“as.factor” forces the variable to become a factor. Otherwise, it would be a chr, 
#or character string.  That is fine for some algorithms, but not all.
#Let’s discard the original column:
data$Sales <- NULL

#Run the “summary” command again.  How many stores have strong sales?
summary(data)
#79 observations have strong sales




########create a simple 80-20 split.##############
set.seed(1234) 
#We are setting the random seed to 1234 to ensure we all get the
#same results from the pseudo random number generator.
ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.8, 0.2))
train <- data[ind==1,]
test <- data[ind==2,]
#################################################



#################################################
#Build a decision tree using the rpart package:
#install.packages("rpart") #If you have not downloaded it already
library(rpart)
help(rpart)
#Let's create a tree:
model.tree <- rpart(StrongSales ~ . , data=train)

summary(model.tree)

#In order to visually inspect the tree, we can use “plot”:
plot(model.tree)
#To add text we need to use:
text(model.tree)

#However, the default options do not display categorical variables very well.  
#I prefer:
plot(model.tree)
text(model.tree, pretty=0)


#We can then use the model to make predictions:
pred.train.tree <- predict(model.tree, train, type = "class")

#Notice that we are passing the model itself, the training data and 
#the “type” flag to the “predict” function.  Setting the flag as “class” tells 
#the function to return a class rather than a probability.
#We can then compute the misclassification rate of the model on training set:
mean(pred.train.tree != train$StrongSales)



#Of course, we should not be interested in the misclassification rate 
#on the training data. What is the misclassification rate on the test data?
#First, we have to create the predictions on the test data
pred.test.tree <- predict(model.tree, test, type = "class")

#We can compute the misclassification rate of the model on testing set:
mean(pred.test.tree != test$StrongSales)

#We can create a confusion matrix using the “table” command:
table(pred.test.tree,test$StrongSales)






#########################################################
#We will now turn our attention to the naïve Bayes model:
#install.packages("e1071") #If you have not downloaded it already
library(e1071)
help(naiveBayes)

#We can create a model similar to the way we created a decision tree:
model.nb <- naiveBayes(StrongSales ~ . , data=train)

#Take a look at the model itself:
model.nb

#Generate the predictions on the training set
NBpreds_train = predict(model.nb,train)

#Generate the predictions on the test set
NBpreds_test = predict(model.nb,test)


#What is the misclassification rate on the training data?
mean(NBpreds_train != train$StrongSales)


#What is the misclassification rate on the testing data?
mean(NBpreds_test != test$StrongSales)


#We can create a confusion matrix using the “table” command:
table(NBpreds_test,test$StrongSales)








#################################################
#Now let us look at nearest neighbor classifiers:
#install.packages("class") #If you have not downloaded it already
library(class)
help(knn)

#KNN does NOT create a model; instead, when given a new test case it simply
#looks through previous observations and returns the predicted class.  
#The function requires:
#     The historical data (in this case, the training data)
#     New cases (in this case, the test data)
#     The correct labels
#     k
#Note: The knn function uses random numbers to break ties.  I am setting 
#the random seed again.
set.seed(1234)
pred.test.knn <- knn(train[,1:5], test[,1:5], train[,11], k = 3)
mean(pred.test.knn != test$StrongSales) 

pred.test.knn <- knn(train[,1:5], test[,1:5], train[,11], k = 5)
mean(pred.test.knn != test$StrongSales)

pred.test.knn <- knn(train[,1:5], test[,1:5], train[,11], k = 10)
mean(pred.test.knn != test$StrongSales)

#Print out the predictions
pred.test.knn

#When K=10, how many times does the classifier correctly predict 'yes'?
#We can create a confusion matrix using the “table” command:
table(pred.test.knn,test$StrongSales)




################################################
#Quiz question #28
#Compare the misclassifaction rates of the training and testing data for the 
#decision tree and naïve Bayes models.  What can you conclude?






################################################
#Quiz question #29
#Give the conditional probability table of Education from the naïve Bayes model.  
#Explain how the algorithm deals with continuous variables.







################################################
#Quiz question #30
#Normalize the numerical data using z-scores and replace the categorical variables with
#dummy variables. Rerun KNN and report results.

######################
#Z-score normalization
library(MASS)
num <- sapply(data, is.numeric)
data[num] <- lapply(data[num], scale)


######################
#Dummy variables
#install.packages("dummies")
library(dummies)
dummydata <- dummy.data.frame(data, sep = ".")
names(dummydata)


########create a simple 80-20 split.##############
set.seed(1234) 
#We are setting the random seed to 1234 to ensure we all get the
#same results from the pseudo random number generator.
ind <- sample(2, nrow(dummydata), replace=TRUE, prob=c(0.8, 0.2))
train <- dummydata[ind==1,]
test <- dummydata[ind==2,]
#################################################


################################################
##########Rerun KNN and report results #########
set.seed(1234)
pred.test.knn <- knn(train[,1:5], test[,1:5], train[,16], k = 3)
mean(pred.test.knn != test$StrongSales.Yes) 

pred.test.knn <- knn(train[,1:5], test[,1:5], train[,16], k = 5)
mean(pred.test.knn != test$StrongSales.Yes)

pred.test.knn <- knn(train[,1:5], test[,1:5], train[,16], k = 10)
mean(pred.test.knn != test$StrongSales.Yes)

#Print out the predictions
pred.test.knn

#When K=10, how many times does the classifier correctly predict 'yes'?
#We can create a confusion matrix using the “table” command:
table(pred.test.knn,test$StrongSales.Yes)




