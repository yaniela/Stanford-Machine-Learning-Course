
plotDecisionBoundary<-function(X,y, theta){
  
  # PLOTDECISIONBOUNDARY Plots the data points X and y into a new figure with
  #   the decision boundary defined by theta
  #   PLOTDECISIONBOUNDARY(theta, X,y) plots the data points in color blue for the
  #   positive examples and color red for the negative examples. X is assumed to be
  #   a either
  #   1) Mx3 matrix, where the first column is an all-ones column for the
  #      intercept.
  #   2) MxN, N>3 matrix, where the first column is all-ones

if (dim(X)[2] <= 3)
  {
  x1 = X[,2]
  x2 = X[,3]
  df = data.frame(x1=x1,x2=x2,y1=y)
  
  plot_x = c(1:100)
  
  plot_y = (-1/theta[3])*(theta[2]*plot_x + theta[1])
  
  ggplot(df, aes(x=x1, y=x2, color=factor(y1, labels=c("No","Yes")))) +  labs(color = "Admited") +
    geom_point(size=2) +  geom_line(color='red', aes(x=plot_x, y=plot_y))  
  
   }
  
  else {
    
    
  }
  
  
}