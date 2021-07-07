# In this part of the exercise, you will implement a neural network to recognize handwritten digits 
# using the same training set working in "MulticlassClasification.R" file.  The neural network will be able 
# to represent complex models that form non-linear hypotheses.  In this exercise you will be using 
# parameters from a neural network that  we  have  already  trained in "ex3weights.dat" file.   
# Your  goal  is  to  implement the  feedforward propagation algorithm to use our weights for prediction. 

## ------------- Part 1: Loading and Visualizing Data --------------
#  We start the exercise by first loading and visualizing the dataset. 
#  You will be working with a dataset that contains handwritten digits.
#

# Load Training Data
cat(sprintf('Loading and Visualizing Data ...\n'))
data <- R.matlab::readMat("ex3data1.mat")
save(data,file = "ex3data1.Rda")
load('ex3data1.Rda')
list2env(data,.GlobalEnv)
rm(data)

m <- dim(X)[1]

# Randomly select 100 images to display
rand_indices <- sample(m)
sel <- X[rand_indices[1:100], ]

source("displayData.R")

displayData(sel)

## ----------------- Part 2: Loading Pameters -----------------
# In this part of the exercise, we load the trained neural network parameters.

cat(sprintf('\nLoading Saved Neural Network Parameters ...\n'))

# Load the weights into variables Theta1 and Theta2
parmeters <- R.matlab::readMat("ex3weights.mat")
save(parmeters ,file = "ex3weights.Rda")
load('ex3weights.Rda')
list2env(parmeters ,.GlobalEnv)
rm(parmeters)

## ------------------- Part 3: Implement Predict -------------------
#  After training the neural network, we would like to use it to predict
#  the labels. You will now implement the "predict" function to use the
#  neural network to predict the labels of the training set. This lets
#  you compute the training set accuracy.

source("sigmoid.R")
source("predictNeuronalNetwrok.R")

pred <- predict(Theta1, Theta2, X)
cat(sprintf('\nTraining Set Accuracy: %f\n', mean(pred==y) * 100))


#  To give you an idea of the network's output, you can also run
#  through the examples one at the a time to see what it is predicting.

#  Randomly permute examples
rp <- sample(m)

for (i in 1:m){
  # Display 
  cat(sprintf('\nDisplaying Example Image. Press Esc to End\n'))
  displayData(X[rp[i], ])
  
  pred <- predict(Theta1, Theta2, X[rp[i],])
  cat(sprintf('\nNeural Network Prediction: %d (y %d) (digit %d)\n', pred ,y[rp[i]],pred%%10))
  
  Sys.sleep(2)
  #press esc to quit the loop in Rstudio
}