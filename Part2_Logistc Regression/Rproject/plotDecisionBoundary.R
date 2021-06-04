
plotDecisionBoundary<-function(data, theta){

  x = data[,1]
  y = data[,2]
  z = data[,3]
  df = data.frame(x1=x,x2=y,y=as.factor(z))
  
  plot_x = c(1:100);
  
  plot_y = (-1/theta[3])*(theta[2]*plot_x + theta[1])
  
  ggplot(df, aes(x=x1, y=x2, color=factor(y, labels=c("No","Yes")))) +  labs(color = "Admited") +
    geom_point(size=2) +  geom_line(color='red', aes(x=plot_x, y=plot_y))  
  
  
  
  
}