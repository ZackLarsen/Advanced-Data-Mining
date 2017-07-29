# CSC 529 Quix #6 Kernel SVM
getwd()
setwd('/Users/zacklarsen/Desktop/CSC 529/Data')
getwd()

kidney <- read.csv('kidney.csv',sep = ',')

#In our investigation of support vector machines, we will use the 
#“kidney” dataset found on the D2L and the “e1071” package in R.  
#This dataset is taken from the UCI Machine Learning Repository 
#(http://archive.ics.uci.edu/ml/datasets/Chronic_Kidney_Disease#).  
#I have cleaned the dataset to fill in missing values and removed columns 
#to make the task more challenging.
#Let us start by looking at the structure of the dataframe:
str(kidney)
# 11 How many observations does the dataset have?
#399

# 12 We can also look at the priors.
prop.table(table(kidney$class))
#What percent of patients have chronic kidney disease? (0.0000)
#0.6240

# 13 As usual, a summary will let us look at center and spread of the variables.
summary(kidney)
#What is the median of “sod”?
#138.0


# 14 Now let us look at some scatter plots.
pairs(kidney[1:6], col=kidney$class, cex=0.5)
#It might help to use the “Zoom” button to get a better look.  
#Looking over the results, there is a clear linear separation between 
#positive and negative cases in the “pot” vs. “age” scatterplot.
# True or false? ____false


# 15 There is a clear linear separation between positive and negative 
#cases in the “wbcc” vs. “sod” scatterplot.
# True or false?  ____false


# 16 Create some simple 80-20 training-test datsets:
set.seed(1234)
ind <- sample(2, nrow(kidney), replace=TRUE, prob=c(0.8, 0.2))
train <- kidney[ind==1,]
test <- kidney[ind==2,]
#How many observations does the training set have?
#324


# 17 The format for creating a support vector machine looks very much like other 
#models you have used:
library(e1071)
svm_model <- svm(class ~ ., data=train)
#And of course, you can get a summary in very much the same way as we have done before.
summary(svm_model)
#According to the summary, how many observations were selected a support vectors?
# 123


# 18 Using “caret” again, we can look at the performance of the svm.
library(caret)
pred <- predict(svm_model,test)
t <- table(pred,test$class)
confusionMatrix(t)
#What is the accuracy of the model? (0.0000)
# 0.9067


# 19 Read the documentation for svm.
help(svm)
#What parameter controls the kernel used in training and predicting?
# kernel


# 20 Create a new svm model with a “linear” kernel.  
#What is the accuracy of this model on the test data?
svm_linear_model <- svm(class ~ ., data=train,kernel='linear')
linear_pred <- predict(svm_linear_model,test)
linear_t <- table(linear_pred,test$class)
confusionMatrix(linear_t)
# Accuracy is 0.84


# 21 Create a new svm model with a “polynomial” kernel.  
#What is the accuracy of this model on the test data?
svm_poly_model <- svm(class ~ ., data=train,kernel='polynomial')
poly_pred <- predict(svm_poly_model,test)
poly_t <- table(poly_pred,test$class)
confusionMatrix(poly_t)
# Accuracy is 0.8267


# 22 Create a new svm model with a “radial” kernel.  
#What is the accuracy of this model on the test data?
svm_radial_model <- svm(class ~ ., data=train,kernel='radial')
radial_pred <- predict(svm_radial_model,test)
radial_t <- table(radial_pred,test$class)
confusionMatrix(radial_t)
# Accuracy is 0.9067


# 23 Create a new svm model with a “sigmoid” kernel.  
#What is the accuracy of this model on the test data?
svm_sigmoid_model <- svm(class ~ ., data=train,kernel='sigmoid')
sigmoid_pred <- predict(svm_sigmoid_model,test)
sigmoid_t <- table(sigmoid_pred,test$class)
confusionMatrix(sigmoid_t)
# Accuracy is 0.84

# 24 Based on your results, the default kernel for the svm function is…
# radial


# 25 Of course, each kernel has several parameters we might want to tune.  
#We will focus on the polynomial model for now.   
#According to the documentation, which parameters affect the polynomial model?


# 26 The e1071 package has a convenient function called “tune.svm”, allowing us to 
#tune several parameters at once:
mytunedsvm <- tune.svm(class ~ ., kernel = "polynomial", data = train, coef0 = (-1:4), degree = (1:4))
summary(mytunedsvm)

# 27 What was the error on the best model?
# 0.09232955


# 28 Reading through a list of numbers can be tedious.  Let us plot the results.
plot (mytunedsvm,xlab="degree", ylab="coef0")
#It looks like a svm model with a polynomial kernel performs better when 
#coef0 is in the range [0,1].


# 29 We will create one last model based on our tuned parameters, 
#choosing a balance between simplicity and accuracy.
svm_model <- svm(class ~ ., data=train, kernel = "polynomial", coef0 = 2, degree = 3)
#What is the accuracy of this model on the test data?
pred <- predict(svm_model,test)
t <- table(pred,test$class)
confusionMatrix(t)
# 0.88


# 30 What improvement did we get by tuning our parameters on the 
#svm with a polynomial kernel?
# 0.88 - 0.8267 = 0.0533
# 0.9067 - 0.88 = 0.0267


