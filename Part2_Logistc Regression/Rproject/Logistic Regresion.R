
#Logistic Regression
# ------------
# we'll build a logistic regression model to predict whether a student gets admitted to a university. 
# For each training example, you have the applicant's scores on two exams and the admissions decision.
# To accomplish this, we're going to build a classification model that estimates the probability of 
# admission based on the exam scores.
# 
# 
library(ggplot2)


# ======================= Part 1:Cargar datos =======================

data= read.table(file = "ex2data1.txt",sep = ",",dec = ".")
names(data)=c("Exam1", "Exam2", "Admited")
head(data)

# =============== Part 2:Graficar datos ====================

summary(data)

data$Admited=as.character(data$Admited)

ggplot(data, aes(x=Exam1, y=Exam2, color=factor(Admited, labels=c("No","Yes")))) + 
  geom_point(size=3)+ labs(color = "Admited")

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

costFunction(theta) #You should see that the cost is about 0.693.


# We do not use the gradient descendent algorthim to calculate the coeff, instead 
# we use the optim function

theta_optim <- optim(par=theta,fn=costFunction)

#set theta
theta <- theta_optim$par

#cost at optimal value of the theta
theta_optim$value

# Prediccion> or  a  student  with  an  Exam  1  scoreof  45  and  an  Exam  2  score  of  85, 
#you  should  expect  to  see  an  admission probability of 0.776


vect=c(1,45, 85 )
predicc=sigmoid(vect%*%theta)
predicc   # I obtain 0.7763541 probability of been Admited

#========= the gradient descendent algorthim to calculate the coeff==============

source("gradienteDescendentLogisticReg.R")

theta= as.matrix(numeric(ncol(data)))

iters=400
alpha=0.00001

result<-gradienteDescendentLogisticReg(X, y, theta, iters, alpha)

vect=c(1,45, 85 )
predicc=sigmoid(vect%*%result[1:3])
predicc       # I obtain 0.6565824 probability of been Admited


#=============== Visualizing decision boundary ===================

x = data[,1]
y = data[,2]
z = data[,3]
df = data.frame(x1=x,x2=y,y=as.factor(z))

plot_x = c(1:100);

plot_y = (-1/theta[3])*(theta[2]*plot_x + theta[1])

ggplot(df, aes(x=x1, y=x2, color=factor(y, labels=c("No","Yes")))) +  labs(color = "Admited") +
  geom_point(size=2) +  geom_line(color='red', aes(x=plot_x, y=plot_y))




