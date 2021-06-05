#Logistic Regression
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

