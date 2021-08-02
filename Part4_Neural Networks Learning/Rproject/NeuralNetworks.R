# In the previous exercise, you implemented feedforward propagation for neu-ral networks and used
# it to predict handwritten digits with the weights weprovided. In this exercise,you will implement
# the backpropagation algorithmtolearnthe parameters for the neural network

## Initialization
rm(list=ls())
sources <- c("checkNNGradients.R", "debugInitializeWeights.R","displayData.R","sigmoid.R","nnCostFunction.R",
             "sigmoidGradient.R", "randInitializeWeights.R", "computeNumericalGradient.R","lbfgsb3_.R")

for (i in 1:length(sources)) {
  cat(paste("Loading ",sources[i],"\n"))
  source(sources[i])
}

## Setup the parameters you will use for this exercise
input_layer_size  <- 400  # 20x20 Input Images of Digits
hidden_layer_size <- 25   # 25 hidden units
num_labels <- 10          # 10 labels, from 1 to 10
# (note that we have mapped "0" to label 10)


## ------------- Part 1: Loading and Visualizing Data --------------
#  We start the exercise by first loading and visualizing the dataset. 



# Load Training Data
cat(sprintf('Loading and Visualizing Data ...\n'))
data <- R.matlab::readMat("ex4data1.mat")
save(data,file = "ex4data1.Rda")
load('ex4data1.Rda')
list2env(data,.GlobalEnv)
rm(data)

m <- dim(X)[1]

# Randomly select 100 images to display
rand_indices <- sample(m)
sel <- X[rand_indices[1:100], ]

displayData(sel)


## ----------------- Part 2: Loading Parameters -----------------
# In this part of the exercise, we load some pre-initialized
# neural network parameters.

cat(sprintf('\nLoading Saved Neural Network Parameters ...\n'))

# Load the weights into variables Theta1 and Theta2
data <- R.matlab::readMat("ex4weights.mat")
save(data,file = "ex4weights.Rda")
load('ex4weights.Rda')
list2env(data,.GlobalEnv)
rm(data)


# Unroll parameters
nn_params <-c(c(Theta1),c(Theta2))


## ----------------- Part 3: Compute Cost (Forward propagation) -----------------

#  To the neural network, you should first start by implementing the
#  feedforward part of the neural network that returns the cost only with the function nnCostFunction.R
#  to return cost. After we can verify that the implementation is correct by verifying that we 
#  get the same cost for the fixed debugging parameters.
#
#  The implementation of the feedforward cost is *without* regularization
#  first so that it will be easier to debug. Later, in part 4, will get to implement the regularized cost.
#

cat(sprintf('\n Forward Propagation Using Neural Network ...\n'))

# Weight regularization parameter (we set this to 0 here).
lambda <- 0


J <- nnCostFunction(input_layer_size, hidden_layer_size,
                    num_labels, X, y, lambda)(nn_params)

cat(sprintf(('Cost at parameters (loaded from ex4weights): %f 
             \n(this value should be about 0.287629)\n'), J))

cat(sprintf('\nProgram paused. Press enter to continue.\n'))
line <- readLines(con = stdin(),1)

## --------------- Part 4: Probe cost function with regularization ---------------

#  Once your cost function implementation is correct, you should now
#  continue to implement the regularization with the cost.
#

cat(sprintf('\nChecking Cost Function (w/ Regularization) ... \n'))

# Weight regularization parameter (we set this to 1 here).
lambda <- 1

J <- nnCostFunction(input_layer_size, hidden_layer_size,
                    num_labels, X, y, lambda)(nn_params)

cat(sprintf('Cost at parameters (loaded from ex4weights): %f
            \n(this value should be about 0.383770)\n', J))

cat(sprintf('Program paused. Press enter to continue.\n'))
line <- readLines(con = stdin(),1)


##### Implementing back propagation algorithm ##########################

## ----------------- Part 5: Sigmoid Gradient  -----------------
#  Before we start implementing the neural network in the sigmoidGradient.R file.
#

cat(sprintf('\nEvaluating sigmoid gradient...\n'))

g <- sigmoidGradient(c(1, -0.5, 0, 0.5, 1))
cat(sprintf('Sigmoid gradient evaluated at [1 -0.5 0 0.5 1]:\n  '))
cat(sprintf('%f ', g))
cat(sprintf('\n\n'))

cat(sprintf('Program paused. Press enter to continue.\n'))
line <- readLines(con = stdin(),1)

## ----------------- Part 6: Initializing Parameters -----------------
# We will be starting to implement a three layer neural network that classifies digits.
#  You will start by implementing a function to initialize the weights of the neural network
#  (randInitializeWeights.R)

cat(sprintf('\nInitializing Neural Network Parameters ...\n'))

initial_Theta1 <- randInitializeWeights(input_layer_size, hidden_layer_size)
initial_Theta2 <- randInitializeWeights(hidden_layer_size, num_labels)

# Unroll parameters
initial_nn_params <- c(initial_Theta1,initial_Theta2)

## --------------- Part 7: Implement Backpropagation ---------------
#  Once your cost matches up with ours, you should proceed to implement the
#  backpropagation algorithm for the neural network. You should add to the
#  code you've written in nnCostFunction.R to return the partial
#  derivatives of the parameters.
#
cat(sprintf('\nChecking Backpropagation... \n'))

#  Check gradients by running checkNNGradients
checkNNGradients()

cat(sprintf('\nProgram paused. Press enter to continue.\n'))
line <- readLines(con = stdin(),1)

## --------------- Part 8: Implement Regularization ---------------
#  Once your backpropagation implementation is correct, you should now
#  continue to implement the regularization (lambda > 0 ) with the cost and gradient.
#

cat(sprintf('\nChecking Backpropagation (w/ Regularization) ... \n'))

#  Check gradients by running checkNNGradients
lambda <-3
checkNNGradients(lambda)

# Also output the costFunction debugging values
debug_J  <- nnCostFunction(input_layer_size,
                           hidden_layer_size, num_labels, X, y, lambda)(nn_params)

cat(sprintf('\n\nCost at (fixed) debugging parameters (w/ lambda <- 10): %f
(this value should be about 0.576051)\n\n', debug_J))

cat(sprintf('Program paused. Press enter to continue.\n'))
line <- readLines(con = stdin(),1)

## -------------------- Part 8: Training NN --------------------
#  You have now implemented all the code necessary to train a neural
#  network. To train our neural network, we will now use "optim" function. Recall that these
#  advanced optimizers are able to train our cost functions efficiently as
#  long as we provide them with the gradient computations.
#
cat(sprintf('\nTraining Neural Network... \n'))

#  You should also try different values of lambda
lambda <- 1

# Create "short hand" for the cost function to be minimized
costFunction <- nnCostFunction(input_layer_size, hidden_layer_size, 
                               num_labels, X, y, lambda) #over nn_params

gradFunction <- nnGradFunction(input_layer_size, hidden_layer_size, 
                               num_labels, X, y, lambda) #over nn_params

# Now, costFunction and gradFunction are functions that take in only one argument (the
# neural network parameters)

optimRes <- optim(par = initial_nn_params, 
                  fn = costFunction, 
                  gr = gradFunction, 
                  method="BFGS", 
                  control = list(maxit = 100))

#With maxit=50 acuracy is 94%
#with maxit=100 accuracy is 97%

nn_params <- optimRes$par
cost <- optimRes$value

# Obtain Theta1 and Theta2 back from nn_params
Theta1 <- matrix(nn_params[1:(hidden_layer_size * (input_layer_size + 1))],
                 hidden_layer_size, (input_layer_size + 1))

Theta2 <- matrix(nn_params[(1 + (hidden_layer_size * (input_layer_size + 1))):length(nn_params)],
                 num_labels, (hidden_layer_size + 1))

cat(sprintf('Program paused. Press enter to continue.\n'))
line <- readLines(con = stdin(),1)

## ------------------- Part 9: Visualize Weights -------------------
#  You can now "visualize" what the neural network is learning by
#  displaying the hidden units to see what features they are capturing in
#  the data.

cat(sprintf('\nVisualizing Neural Network... \n'))

displayData(Theta1[, -1])

cat(sprintf('\nProgram paused. Press enter to continue.\n'))
line <- readLines(con = stdin(),1)

## ------------------- Part 10: Implement Predict -------------------
#  After training the neural network, we would like to use it to predict
#  the labels. You will now implement the "predict" function to use the
#  neural network to predict the labels of the training set. This lets
#  you compute the training set accuracy.

source("predict.R")

pred <- predict(Theta1, Theta2, X)

cat(sprintf('\nTraining Set Accuracy: %f\n', mean(pred==y) * 100))

cat('Program paused. Press enter to continue.\n')
line <- readLines(con = stdin(),1)

#  To give you an idea of the network's output, you can also run
#  through the examples one at the a time to see what it is predicting.

#  Randomly permute examples
rp <- sample(m)

for (i in 1:m){
  # Display
  cat(sprintf('\nDisplaying Example Image. Press Esc to End\n'))
  displayData(X[rp[i], ])
  
  pred <- predict(Theta1, Theta2, X[rp[i],])
  cat(sprintf('\nNeural Network Prediction: %d (y %d) (digit %d)\n', pred  , y[rp[i]]  ,pred %% 10))
  
  # line <- readLines(con = stdin(),1)
  #cat(sprintf('Program paused. Press enter to continue.\n')
  #line <- readLines(con = stdin(),1)
  Sys.sleep(2)
  #press esc to quit the loop in Rstudio
}
