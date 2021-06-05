predict <- function(theta, X) {
  #PREDICT Predict whether the label is 0 or 1 using learned logistic
  #regression parameters theta
  #   p <- PREDICT(theta, X) computes the predictions for X using a
  #   threshold at 0.5 (i.e., if sigmoid(theta'*x) >= 0.5, predict 1)
  
  m <- dim(X)[1] # Number of training examples
  p <- rep(0,m)
  p[sigmoid(X %*% theta) >= 0.5] <- 1
  p
  # ----------------------------------------------------
}