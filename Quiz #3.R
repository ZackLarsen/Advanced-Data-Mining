#In this tutorial, we are going to explore various techniques to evaluate models. 
#Let us begin by loading the "crash" dataset found on the D2L. The first column 
#(ACCIDENT) describes whether a customer has been in an accident 
#(filed and insurance claim). Imagining that we work for the insurance company, 
#we want to build a classifier to predict who is likely to make claims using features 
#such as marital status, income or how many children live at home. We will begin with 
#a simple decision tree:

getwd()
setwd('/Users/zacklarsen/Desktop/CSC 529/Data')
getwd()

crash <- read.csv('crash.csv',sep = ' ')


set.seed(1234)
ind <- sample(2, nrow(crash), replace=TRUE, prob=c(0.8, 0.2))
train <- crash[ind==1,]
test <- crash[ind==2,]

library(rpart)

model.tree <- rpart(ACCIDENT ~ . , data=train)
pred.model.tree <- predict(model.tree, test, type = "class")
table(pred.model.tree,test$ACCIDENT)

# Question 11 How many “YES” cases does the tree correctly classify?
# 93 cases

# Question 12 Accuracy is the percent of correct predictions. 
#Using the confusion matrix calculate accuracy.
#  Accuracy = TP + TN / (TP+TN+FP+FN) = (93+1152)/(1152+314+62+93) = 0.7680444


# Question 13 Recall (or sensitivity) is the fraction of “YES” cases that are successfully 
#predicted: TP / (TP + FN).  Using the confusion matrix calculate recall.
# Recall = (93) / (93+314) = 0.2285012


# Question 14 Precision (positive predicted value) is the fraction of “YES” predictions that 
#were correct: TP / (TP + FP).  Using the confusion matrix calculate recall.
# Precision = (93) / (93+62) = 0.6


# Question 15 Thankfully, we do not need to calculate the performance metrics by hand.  
#R has a nice package that can do it all for us.
#install.packages("caret")
library(caret)
t <- table(pred.model.tree,test$ACCIDENT)
confusionMatrix(t)


#Look through the output carefully.  At the end you will see, “Positive Class : NO“.
#R takes the first level it sees as the positive class.  You can expand the data description of “crash” in the Environment pane.  What is the first Factor listed for the variable “ACCIDENT”?

#We can force “YES” to be the first level in the factor:
crash$ACCIDENT <- factor( crash$ACCIDENT, levels=c("YES","NO") )

#Rerun the code to build the training/test data (including setting the random seed)
#and rebuild the decision tree.
set.seed(1234)
ind <- sample(2, nrow(crash), replace=TRUE, prob=c(0.8, 0.2))
train <- crash[ind==1,]
test <- crash[ind==2,]
model.tree <- rpart(ACCIDENT ~ . , data=train)
pred.model.tree <- predict(model.tree, test, type = "class")
table(pred.model.tree,test$ACCIDENT)
t <- table(pred.model.tree,test$ACCIDENT)
confusionMatrix(t)

#What does caret report for the recall (sensitivity)?
#0.22850

#Notice that caret also reports the confidence interval for accuracy.  
#What is the lower bound?
#0.7467


#Let us review how this is calculated.  You will remember that a confidence interval
#is computed as:
#mean +/- margin of error
#In this scenario, we are assuming a Bernoulli distribution.  In probability theory 
#and statistics, the Bernoulli distribution is the probability distribution of a 
#random variable which takes the value 1 with success probability of p and the 
#value 0 with failure probability of q = 1 - p.
#Success, in this case, means a correct prediction.  We can use accuracy on the test 
#dataset as an approximation for the true accuracy of the classifier
#So, p = accuracy
#What is q?
p = 0.768
q = (1-p)
q

#When inferring the confidence interval for a proportion, the standard error is 
#computed as:
#standard error = sqrt(p * q / n)
#What is the standard error?
n = length(test$ACCIDENT)
n
se = sqrt((p*q)/n)
se


#If we want to compute the 95% confidence interval, what z should we use? (x.xx)
z = 1.96


#Finally, you will remember that:
#margin of error = z * standard error
#What is the margin of error?
moe = z*se
moe


p
p-moe
MOE <- p-0.7467
MOE
MOE/1.96



#Now, let us turn our attention to n-fold cross validation. Once again, we will
#use the caret package:
set.seed(1234)
train_control <- trainControl(method="cv", number=10)
model <- train(ACCIDENT~., data=crash, trControl=train_control, method="rpart")
pred.cv.tree <- predict(model,crash)
t <- table(pred.cv.tree,crash$ACCIDENT)
confusionMatrix(t)

#Read the documentation of “train” and “trainControl”. If we wanted to perform
#bootstrapping which variable, would we change?
#   method

#If we wanted to change the number of folds, which variable would we change?
# number

#What is the accuracy based on the n-fold cross validation evaluation of "rpart"?
#0.7651


#N-fold cross validation has greatly increased our n!  Notice that caret is
#now reporting a much smaller margin of error for the accuracy of the model.
#What is it?
# moe = accuracy-lower bound of CI = 0.7651-0.7557
0.7651-0.7557





#Let us now create a k-nn model.  (It may take a minute to run).
set.seed(1234)
train_control <- trainControl(method="cv", number=10)
knnFit <- train(ACCIDENT ~ ., data = crash, method = "knn", trControl = train_control, preProcess = c("center","scale"), tuneLength = 20)
knnFit

#The caret package not only performed n-fold cross validation, but tuned k for 
#the k-nn algorithm!
#What k was chosen?
# k = 15

#We can use caret to ouput the performance as before:
pred.cv.tree <- predict(knnFit,crash)
t <- table(pred.cv.tree,crash$ACCIDENT)
confusionMatrix(t)

#What is the accuracy?
# Accuracy = 0.7875

#True or False, based on the confidence intervals, we can be nearly certain
#that the k-nn model outperforms the decision tree model wrt. accuracy.
# False-although the accuracy is higher than the accuracy for the previous model,
# it is not above the margin of error so there is uncertainty involved for the first
# decision tree. The cross-validation decision tree was definitely lower accuracy,
# but not the first decision tree.
#CI from first decision tree was [0.7467, 0.7884]
 


















































