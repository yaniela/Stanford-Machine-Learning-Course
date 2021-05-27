# Regresion lineal simple   
# ------------
# 
# x refers to the population size in 10,000s
# y refers to the profit in $10,000s
# 
# 

# ======================= Part 1:Cargar datos =======================

data= read.table(file = "ex1data1.txt",sep = ",",dec = ".")
names(data)=c("population", "profit")
head(data)

# =============== Part 2:Graficar datos ====================

summary(data)
plot(data, ylab="Profit in 10,000", xlab="Population of City in 10,000")


# ============= Part 3: Cargar la funcion de costo y algoritmo Gradiente Descendente ================

source("computeCost.R")
source("gradienteDescendente.R")

# ===== Part 4: Calculando los coeficientes ===============

#Agregar a la tabla una columna con 1s para realizar la operacion
#entre matrices para el primer coeficiente.

X=as.matrix(cbind(rep(1, nrow(data)),data[,1]))
y=as.matrix(data[,2])
theta=as.matrix(c(0,0))  #Los coeficientes iniciales son 0 y 0


computeCost(X,y, theta) #Probando el Costo inicial= 32.07

alpha = 0.01
iters = 2500 # Con 2500 iteraciones se acerca bastante a la linea que devuelve la función de R

result<-gradienteDescendente(X, y, theta, alpha, iters)

# ============================================================
# Graficando el error vs iteraciones y se observa que disminuye

plot(c(0:iters+1), result[3:length(result)], type="l", xlab = "Iteraciones", ylab="Error") 

# ============================================================
# Graficar la regresion con mis coeficientes

myfit=c(result[1], result[2])


#===== Part 6: Comprobando las predicciones respecto a las funciones de R =============== 

#Your final values for θ will also be used to make predictions on profits in areas 
#of 35,000 and 70,000 people

fit=lm(data$profit~data$population)
plot(data, ylab="Profit in 10,000", xlab="Population of City in 10,000")
abline(fit, col="blue")
abline(myfit, col="red")

#===== Part 7: Prediccion con dos valores =============== 

mypred1=myfit[1]+myfit[2]*35.000
mypred2=myfit[1]+myfit[2]*70.000


pred1R=fit$coefficients[1]+fit$coefficients[2]*35.000
pred2R=fit$coefficients[1]+fit$coefficients[2]*70.000

cat("Predicc para poblacion de 37,000. Prediccion de R:", pred1R, "Mi prediccion: ", mypred1)
cat("Predicc para poblacion de 70,000. Prediccion de R:", pred2R, "Mi prediccion: ", mypred2)
