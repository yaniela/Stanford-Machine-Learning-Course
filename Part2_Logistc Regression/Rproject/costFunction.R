
source("sigmoid.R")

costFunction<-function(theta){
  
  Z=sigmoid(X%*%theta)
  
  temp=(-y*log(Z))-((1-y)*(log(1-Z)))
  
  return((1/length(y))*sum(temp))
}