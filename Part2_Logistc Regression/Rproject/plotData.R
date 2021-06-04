plotData<-function(data)
{
  ggplot(data, aes(x=Exam1, y=Exam2, color=factor(Admited, labels=c("No","Yes")))) + 
    geom_point(size=3)+ labs(color = "Admited")
  
}