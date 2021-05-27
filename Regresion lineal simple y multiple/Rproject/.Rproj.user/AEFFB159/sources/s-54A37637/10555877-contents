gradienteDescendenteRegMultiple<-function (X, y, theta, alpha, iters){
  
  m = length(y); # number of training examples
  J_history = c(computeCost(X, y, theta))
  
  for (iter in 1:iters){
    
    temp = numeric(nrow(theta))
    
    for (i in 1:nrow(theta)){
      
      temp[i] = temp[i] + sum(((X%*%theta)-y)*X[,i])
    }
    
    for (j in 1:nrow(theta)){
      
      theta[j] = theta[j] - ((alpha/m)*temp[j])
    }
    
    J_history = c(J_history, computeCost(X, y, theta))
    
  }
  
  
  result=c(theta, J_history)
  return(result)
}