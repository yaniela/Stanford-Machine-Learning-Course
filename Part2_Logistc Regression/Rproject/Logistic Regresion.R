## Machine Learning Online Class - Exercise 2: Logistic Regression
#
# we'll build a logistic regression model to predict whether a student gets admitted to a university. 
# For each training example, you have the applicant's scores on two exams and the admissions decision.
# To accomplish this, we're going to build a classification model that estimates the probability of 
# admission based on the exam scores.
# 
# 

## Initialization

library(ggplot2)

# ======================= Part 1:Load Data =======================

data= read.table(file = "ex2data1.txt",sep = ",",dec = ".")
names(data)=c("Exam1", "Exam2", "Admited")
head(data)

summary(data)
data$Admited=as.character(data$Admited)

# =============== Part 2:Graficar datos ====================

cat(sprintf('Plotting data with + indicating (y = 1) examples and o indicating (y = 0) examples.\n'))

ggplot(data, aes(x=Exam1, y=Exam2, color=factor(Admited, labels=c("No","Yes")))) + 
  geom_point(size=3)+ labs(color = "Admited")

cat(sprintf('\nProgram paused. Press enter to continue.\n'))

#=============== check to make sure the sigmoid function is working. ====================

source("sigmoid.R")

sigmoid(0)

plot(c(-10:10), sigmoid(c(-10:10)), type="l")

#============Now we need to do some setup============

y=as.matrix(as.numeric(data[,3]))

theta= as.matrix(numeric(ncol(data)))
X=as.matrix(cbind(numeric(nrow(data)),data[,1:2]))
X[,1]=1

source("costFunction.R")

cost<-costFunction(theta) #You should see that the cost is about 0.693.


# We do not use the gradient descendent algorthim to calculate the coeff, instead 
# we use the optim function

theta_optim <- optim(par=theta,fn=costFunction)

#set theta
theta <- theta_optim$par

#cost at optimal value of the theta


# Print theta to screen
cat(sprintf('Cost at theta found by optim: %f\n', theta_optim$value))
cat(sprintf('theta: \n'))
cat(sprintf(' %f \n', theta))


# Prediction of  a  student  with  an  Exam  1  scoreof  45  and  an  Exam  2  score  of  85, 
#you  should  expect  to  see  an  admission probability of 0.776


vect=c(1,45, 85 )
prob=sigmoid(vect%*%theta)

cat(sprintf('For a student with scores 45 and 85, we predict an admission probability of\n %f\n', prob))

#=============== Visualizing decision boundary ===================

source("plotDecisionBoundary.R")

plotDecisionBoundary(X,y, theta)

#========== Compute accuracy on our training set=============

source("predict.R")

p <- predict(theta, X)

cat(sprintf('Train Accuracy: %f\n', mean(p == y) * 100))

#############

