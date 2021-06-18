# For  this  exercise,  you  will  use  logistic  regression  and  neural  networks 
# to recognize  hand written  digits  (from  0  to  9).   Automated  hand written  
# digit recognition is widely used today - from recognizing zip codes (postal codes)on  mail  
# envelopes  to  recognizing  amounts  written  on  bank  checks.   This exercise will show you how the
# methods you’ve learned can be used for this classification task.In the first part of the exercise, you 
# will extend your previous implementation of logistic regression and apply it to one-vs-all classification.

# Setup the parameters you will use for this part of the exercise
input_layer_size  <- 400  # 20x20 Input Images of Digits
num_labels <- 10          # 10 labels, from 1 to 10   
# (note that we have mapped "0" to label 10)


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

# Randomly select 100 data points to display
rand_indices <- sample(m)
sel <- X[rand_indices[1:100], ]

source("displayData.R")

displayData(sel)



## ------------ Part 2: Vectorize Logistic Regression ------------
# Now will reuse the logistic regression code from the last exercise. 
# Your task here is to make sure that your
#  regularized logistic regression implementation is vectorized. After
#  that, you will implement one-vs-all classification for the handwritten
#  digit dataset.
#
source("sigmoid.R")
source("lrCostGradFunction.R")
source("OneVsAll.R")

cat(sprintf('\nTraining One-vs-All Logistic Regression...\n'))

lambda <- 0.1
all_theta <- oneVsAll(X, y, num_labels, lambda)

cat(sprintf('Program paused. Press enter to continue.\n'))
line <- readLines(con = stdin(),1)

## ----------------- Part 3: Predict for One-Vs-All -----------------
source("predictOneVsAll.R")

pred <- predictOneVsAll(all_theta, X)

cat(sprintf('\nTraining Set Accuracy: %f\n', mean(pred == y) * 100))