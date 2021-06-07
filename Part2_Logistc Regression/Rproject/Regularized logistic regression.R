#Regularized Logistic Regression
# ------------
# we'll build a logistic regression model to predict whether a student gets admitted to a university. 
# For each training example, you have the applicant's scores on two exams and the admissions decision.
# To accomplish this, we're going to build a classification model that estimates the probability of 
# admission based on the exam scores.
# Suppose you are the product manager of the factory and you have the test results for some microchips 
# on two different tests.  From these two tests,you would like to determine whether the microchips should
# be accepted orrejected.  To help you make the decision, you have a dataset of test results on past 
# microchips, from which you can build a logistic regression model
# # 


library(ggplot2)



# ======================= Part 1:Cargar datos =======================

data= read.table(file = "ex2data2.txt",sep = ",",dec = ".")
names(data)=c("Test1", "Test2", "Acepted")
head(data)
data
# =============== Part 2:Graficar datos ====================

summary(data)

data$Acepted=as.character(data$Acepted)

ggplot(data, aes(x=Test1, y=Test2, color=factor(Acepted, labels=c("No","Yes")))) + 
  geom_point(size=3)+ labs(color = "Acepted")

# Figure shows that our dataset cannot be separated into positive and negative examples
# by a straight-line through the plot.  Therefore, a straight-forward application of 
# logistic regression will not perform well on this dataset since logistic regression will
# only be able to find a linear decision boundary.

#One  way  to  fit  the  data  better  is  to  create  more  features  from  each  data point.
#In the provided function mapFeature.R, we will map the features into all polynomial terms of x1 and x2
#up to the sixth power.


# As  a  result  of  this  mapping,  our  vector  of  two  features  (the  scores  on two QA tests)
# has been transformed into a 28-dimensional vector.  A logistic regression classifier trained on this 
# higher-dimension feature vector will have a more complex decision boundary and will appear nonlinear 
# when drawn in our 2-dimensional plot.


#========== Part3: Add Polynomial Features =========

source("mapFeature.R")

X <- mapFeature(data[,1], data[,2])

y <- as.numeric(data[,3])




# Initialize fitting parameters
initial_theta <- rep(0,dim(X)[2])

# Set regularization parameter lambda to 1
lambda <- 1

# Compute and display initial cost and gradient for regularized logistic
# regression
source("costFunctionReg.R")

cost <- costFunctionReg(X, y, lambda)(initial_theta)
grd <- gradReg(X,y, lambda)(initial_theta)
cat(sprintf('Cost at initial theta (zeros): %f\n', cost))

cat(sprintf('\nProgram paused. Press enter to continue.\n'))
line <- readLines(con = stdin(),1)

#========== Part4: Regularization and Accuracies --------------
#  Optional Exercise:
#  In this part, you will get to try different values of lambda and
#  see how regularization affects the decision boundary
#
#  Try the following values of lambda (0, 1, 10, 100).
#
#  How does the decision boundary change when you vary lambda? How does
#  the training set accuracy vary?
#

library(gridExtra)

source("plotDecisionBoundary.R")

myplots <- vector('list', length=4)
mythetas<- vector('list', length=4)
mycost<- vector('list', length=4)

values<-c(1,0,100,155)

for(i in seq_along(values) ){

# Initialize fitting parameters
initial_theta <- rep(0,dim(X)[2])

# Set regularization parameter lambda to 1 (you should vary this)
lambda <- values[i]

#try with lambda in (1,0,100,155)
# Optimize
optimRes <- optim(par = initial_theta, 
                  fn = costFunctionReg(X,y,lambda), 
                  gr = gradReg(X,y,lambda), 
                  method="BFGS", 
                  control = list(maxit = 400))

theta <- optimRes$par
J <- optimRes$value

mythetas[[i]]<-theta
mycost[[i]]<-J

plot<-plotDecisionBoundary(X,y,theta, lambda)
myplots[[i]] <- plot

}

grid.arrange(grobs=myplots, ncol = 2,nrow=2)



#% Compute accuracy on our training set

source("predict.R")


m <- data.frame(matrix(0, ncol = 3, nrow = 4))
colnames(m)<-c("lambda","cost", "%accuracy")

 for(i in seq_along(mycost)){
   
   p = predict(mythetas[[i]], X)
   acc = mean(p == y) * 100
   
   m[i,1]<-values[i]
   m[i,2]<-mycost[[i]]
   m[i,3]<-acc
   
 }

print("Train Accuracy:")

m
