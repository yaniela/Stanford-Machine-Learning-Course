#  Linear regression with multiple variables

# The fileex1data2.txt contains a training set of housing prices
# in Port-land, Oregon.  The first column is the size of the house 
# (in square feet), the second column is the number of bedrooms, and 
# the third column is the price of the house.


# ======================= Part 1:Cargar datos =======================

data= read.table(file = "ex1data2.txt",sep = ",",dec = ".")
names(data)=c("size", "number of bedrooms", "price")
head(data)

# ================ Part 2: Feature Normalization ================


X=as.matrix(cbind(data[,1],data[,2]))
y=as.matrix(data[,3])

for(i in 1:ncol(X)){
  
 X[,i]=(X[,i]-mean(X[,i]))/sd(X[,i]) 
 
}

head(X)

theta= as.matrix(numeric(ncol(X)+1))
X=as.matrix(cbind(numeric(nrow(X)),X))
X[,1]=1

# ============= Part 3: Cargar la funcion de costo y algoritmo Gradiente Descendente ================

source("computeCost.R")
source("gradienteDescendenteRegMultiple.R")


# ================ Part 4: Finding the best learning rate ================

# Notice the changes in the convergence curves as the learning rate changes.
# With a small learning rate (alpha), you should find that gradient descent takes a very long 
# time to converge to the optimal value.  Conversely, with a large learning rate,
# gradient descent might not converge or might even diverge


alpha = 0.1
iters = 500 


result<-gradienteDescendenteRegMultiple(X, y, theta, alpha, iters)


plot(c(0:iters+1), result[4:length(result)], type="l", xlab = "Iteraciones", ylab="Error")

# ================ Part 5: Obteniendo los coeficientes y realizando a prediccion ================

# Using the best learning rate that you found, use this value of theta to predict 
# the price of a house with 1650 square feet and 3 bedrooms

#Don’t forget to normalize your features when you make this prediction!


coeff=result[1:3]

size=(1650-mean(data[,1]))/sd(data[,1])
noBedrodms=(3-mean(data[,2]))/sd(data[,2])

vect=c(1,size, noBedrodms )
predicc=sum(vect*coeff)


#================ Part 6: Predicción con funcion en R vs Mi prediccion ================

fit=lm(data$price~data$size+data$`number of bedrooms`)

predR=fit$coefficients[1]+fit$coefficients[2]*1650 + fit$coefficients[3]*3

cat("Predicc R: ", predR, "My predicc: ", predicc)
