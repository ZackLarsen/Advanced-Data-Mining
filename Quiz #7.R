# CSC 529 Quix #7 Kernel PCA
getwd()
setwd('/Users/zacklarsen/Desktop/CSC 529/Data')
getwd()

shapes <- read.csv('shapes.csv',sep = ',')

#This tutorial will use the “shapes” dataset on the D2L.  
#We will use the “kernlab” and “RColorBrewer” packages.

#We will first set up our color palette.

library(RColorBrewer)
display.brewer.all()
colors <- brewer.pal(10, "Spectral")

#There are no classes in this dataset, but we will use the colors to 
#keep track of points before and after transformations.

# 10 How many colors are variable in RColorBrewer's spectral palette 
#(including the last “blank” color)?



# 11 The dataset on the D2L has four sets of artificially generated data: 
#corner, cinc, moons and spirals.  For example, the corner data has 
#cornerX, cornerY and cornerC to describe the x and y coordinate as 
#well as a label for the color pallete.  Let us plot this data.
plot(shapes$cornerX, shapes$cornerY, col=colors[shapes$cornerC])
#What color is the top right group of points?
# Yellow



# 12 We can use “princomp” to compute the new PCA transformation on this data.
pc.corner <- princomp(shapes[,1:2], cor=TRUE, scores=TRUE)
plot(pc.corner$scores, col=colors[shapes$cornerC])
#PCA found the transformation that captures the most variability of the data.  
#However, it did not capture the structure of the data very well.
#What color is the bottom most group of points?
# Yellow



# 13 We will now attempt kernel PCA, for which you will need the “kernlab” package.  
#(Note: kernlab uses S4 objects which is worth googling if you are unfamiliar with them.)
library(kernlab)
kpc.corner <- kpca(~., data=shapes[,1:2], kernel="rbfdot", kpar=list(sigma=0.2), 
                   features=5)
#We are using a Bayesian kernel with a sigma of 0.2.  According to the documentation,
#how many kernels are built into kernlab?
# 7



# 14 Similar to standard PCA we can look at the Eigen values.
kpc.corner@eig
#What is the Eigen value of the first component?
# 0.06375426


# 15 We can also look at the principle component vectors.
kpc.corner@pcv
#Notice we did not compute two principle components even though we have 
#just two original variables.  Kernel PCA is building a higher order space 
#before deriving the components. How many components did we compute?
# 5



# 16 Plotting these new features reveals that the components capture some
#of the structural information from the original graph.
plot(kpc.corner@pcv[,c(1,2)], col=colors[shapes$cornerC])
#The first kernel principle component does an excellent job at distinguishing 
#which of the original corners from the others?
# yellow



# 17 Looking at kpc 1 vs. kpc 3…
plot(kpc.corner@pcv[,c(1,3)], col=colors[shapes$cornerC])
#..we can see that the third kernel principle component does an excellent job
#splitting which two of the original corners?
# yellow and orange






# 18 Use the cinc data from the shapes dataset and 
#1) plot the original data, 
plot(shapes$cincX, shapes$cincY, col=colors[shapes$cincC])
#2) plot the data after performing PCA and 
pc.cinc <- princomp(shapes[,4:5], cor=TRUE, scores=TRUE)
plot(pc.cinc$scores, col=colors[shapes$cincC])
#3) plot the data after performing kernel PCA. 
kpc.cinc <- kpca(~., data=shapes[,4:5], kernel="rbfdot", kpar=list(sigma=0.2), 
                   features=5)
kpc.cinc@eig
kpc.cinc@pcv
plot(kpc.cinc@pcv[,c(1,2)], col=colors[shapes$cincC])

#   Experiment with different kernels and different parameters.
#Post your work to the D2L.
#Mark T when done.  (I have asked my grader to double check that you have done so.)






# 19 Use the moons data from the shapes dataset and 
#1) plot the original data, 
plot(shapes$moonsX, shapes$moonsY, col=colors[shapes$moonsC])

#2) plot the data after performing PCA and 
pc.moons <- princomp(shapes[,7:8], cor=TRUE, scores=TRUE)
plot(pc.moons$scores, col=colors[shapes$moonsC])

#3) plot the data after performing kernel PCA.  
kpc.moons <- kpca(~., data=shapes[,7:8], kernel="rbfdot", kpar=list(sigma=0.2), 
                 features=5)
kpc.moons@eig
kpc.moons@pcv
plot(kpc.moons@pcv[,c(1,2)], col=colors[shapes$moonsC])

#   Experiment with different kernels and different parameters.
#Post your work to the D2L.
#Mark T when done.  (I have asked my grader to double check that you have done so.)








# 20 Use the spiral data from the shapes dataset and 
#1) pplot(shapes$cornerX, shapes$cornerY, col=colors[shapes$cornerC])
plot(shapes$spiralX, shapes$spiralY, col=colors[shapes$spiralC])

#2) plot the data after performing PCA and 
pc.spiral <- princomp(shapes[,10:11], cor=TRUE, scores=TRUE)
plot(pc.spiral$scores, col=colors[shapes$spiralC])

#3) plot the data after performing kernel PCA.  
kpc.spiral <- kpca(~., data=shapes[,10:11], kernel="rbfdot", kpar=list(sigma=0.2), 
                 features=5)
kpc.spiral@eig
kpc.spiral@pcv
plot(kpc.spiral@pcv[,c(1,2)], col=colors[shapes$spiralC])


#   Experiment with different kernels and different parameters.
#Post your work to the D2L.
#Mark T when done.  (I have asked my grader to double check that you have done so.)















































































