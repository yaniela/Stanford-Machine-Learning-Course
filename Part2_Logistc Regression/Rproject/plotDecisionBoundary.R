library(tidyverse)

plotDecisionBoundary<-function(X,y, theta, lambda){
  
  # PLOTDECISIONBOUNDARY Plots the data points X and y into a new figure with
  # the decision boundary defined by theta. Lambda value is only useful in regularized logistic regression, 
  # #   PLOTDECISIONBOUNDARY(theta, X,y) plots the data points in color blue for the
  #   positive examples and color red for the negative examples. X is assumed to be
  #   a either
  #   1) Mx3 matrix, where the first column is an all-ones column for the
  #      intercept.
  #   2) MxN, N>3 matrix, where the first column is all-ones

  x1 = X[,2]
  x2 = X[,3]
  df = data.frame(x1=x1,x2=x2,y1=as.character(y))
  

  if (dim(X)[2] <= 3)
  {
  
  plot_x = c(1:100)
  
  plot_y = (-1/theta[3])*(theta[2]*plot_x + theta[1])
  
  ggplot(df, aes(x=x1, y=x2, color=factor(y1, labels=c("No","Yes")))) +  labs(color = "Admited") +
    geom_point(size=2) +  geom_line(color='red', aes(x=plot_x, y=plot_y))  
  
   }
  
  else {
    
    # Here is the grid range
    u <- seq(-1,1.5, length.out = 50)
    v <- seq(-1,1.2, length.out = 50)
    
    z <- matrix(0, length(u), length(v))
    # Evaluate z <- theta*x over the grid
    for (i in 1:length(u))
      for (j in 1:length(v))
        z[i,j] <- mapFeature(u[i], v[j]) %*% theta
    
    
    rownames(z) <- u #or some vector like u
    colnames(z) <- v #or soem vector like v
    
    as.data.frame(z) %>% 
      rownames_to_column() %>% 
      gather(key, value, -rowname) %>% 
      mutate(key = as.numeric(key), 
             rowname = as.numeric(rowname)) %>%
      ggplot() +
      geom_contour(bins = 2, aes(x = rowname, y = key, z = value)) +    xlab(label = "Test1") +
      ylab(label = "Test2") +
      geom_point(data = df,aes(x = x1, y = x2, color=factor(y1, labels=c("No","Yes"))) )  + labs(tag =paste("\u03BB","=",lambda) ,color = "Admited") 
      
    
    
  }
  
  
}