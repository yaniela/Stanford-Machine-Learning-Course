computeCost<- function(X, y, theta) {
  
  aux=((X%*%theta)-y)^2
  result=sum(aux)/(2*length(y))
  result
}