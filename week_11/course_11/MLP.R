#An Introduction To Deep Learning 
#Chapter 4 Code Examples 

#Clear the workspace 
rm(list = ls())

#Loading the required packages
require(monmlp)
require(Metrics)

#MultiLayer Perceptron Code
x <- as.matrix(seq(-10, 10, length = 100))
y <- logistic(x) + rnorm(100, sd = 0.2)

#Plotting Data
plot(x, y)
lines(x, logistic(x), lwd = 10, col = "gray")

#Fitting Model
mlpModel <- monmlp.fit(x = x, y = y, hidden1 = 3, monotone = 1,
                       n.ensemble = 15, bag = TRUE)
mlpModel <- monmlp.predict(x = x, weights = mlpModel)

#Plotting predicted value over actual values
for(i in 1:15){
  lines(x, attr(mlpModel, "ensemble")[[i]], col = "red")
}

cat ("MSE for Gradient Descent Trained Model: ", mse(y, mlpModel))

####################################################################
#Conjugate Gradient Trained NN
#install.packages('RSNSS')
require(RSNNS)

#About mlp function https://www.rdocumentation.org/packages/RSNNS/versions/0.4-10/topics/mlp
conGradMLP <- mlp(x = x, y = y, 
                  size = (2/3)*nrow(x)*2, 
                  maxit = 200, 
                  learnFunc = "SCG")

y_h <- predict(conGradMLP, x)

cat ("MSE for Conjugate Gradient Descent Trained Model: ", mse(y_h, y))

