gradienteDescendente<-function (X, y, theta, alpha, iters){
  
  
  m = length(y); # number of training examples
  J_history = c(computeCost(X, y, theta))
  cat( "Compute cost:", computeCost(X, y, theta))
  
  for (iter in 1:iters){
    
    temp0 = 0;
    temp1 = 0;
    
    for (i in 1:m){
      temp0 = temp0 +((X[i,1]*theta[1,1] + X[i,2]*theta[2,1]) - y[i,1])
      temp1 = temp1 + ((X[i,1]*theta[1,1] + X[i,2]*theta[2,1]) - y[i,1])*X[i,2]
    }
    
    theta[1,1] = theta[1,1] - ((alpha/m)*temp0)
    theta[2,1] = theta[2,1] - ((alpha/m)*temp1)
    
    
    
    # ============================================================
    
    #Save the cost J in every iteration    
    J_history = c(J_history, computeCost(X, y, theta))
    
    cat( "Compute cost:", computeCost(X, y, theta))
  }
  result=c(theta, J_history)
  return(result)
}