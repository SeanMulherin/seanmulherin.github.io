######### Initial ######### 
set.seed(1)
library(dplyr)
library(nnet)
library(NeuralNetTools)

data(iris)
iris$Species <- as.integer(iris$Species) 


######### cross validation ######### 
n <- nrow(iris)
rows <- sample(1:n, 0.8 * n)

train <- iris[rows, ]
train <- as.matrix(train[ , 1:4])

test <- iris[-rows, ]
test <- as.matrix(test[ , 1:4])

colmax <- apply(iris[,1:4], 2, max) # standardize step
train <- t( t(train) / colmax)
test <- t( t(test) / colmax)

X <- train


######### creates vector of 1/0's  ######### 
a <- matrix(0, nrow = nrow(iris), ncol = 3)
a[, 1] <- iris$Species

iris$Species <- matrix(a, nrow = nrow(iris), ncol = 3)

for(i in 1:nrow(iris)){
  if(2 %in% iris$Species[i, ]) {
    iris$Species[i, ] <- c(0, 1, 0)
  } else if(3 %in% iris$Species[i, ]) {
    iris$Species[i, ] <- c(0, 0, 1)
  }
}

Y <- iris$Species[rows, ]
Y_test <- iris$Species[-rows, ]


######### activation function - should use reLU ######### 
sigmoid <- function(Z){
  1/(1 + exp(-Z))
}

### forward feed
W_1 <- matrix(runif(12), nrow = 4, ncol = 3)
W_2 <- matrix(runif(9), nrow = 3, ncol = 3)
Z_2 <- X %*% W_1
A_2 <- sigmoid(Z_2)
Z_3 <- A_2 %*% W_2
Y_hat <- sigmoid(Z_3)



######### Calculate Gradient - partial derivatives  ######### 
sigmoidprime <- function(z){
  exp(-z)/((1 + exp(-z))^2)
}

delta_3 <- -(Y - Y_hat) * sigmoidprime(Z_3)
djdw2 <- t(A_2) %*% delta_3

delta_2 <- delta_3 %*% t(W_2) * sigmoidprime(Z_2)
djdw1 <- t(X) %*% delta_2



######### Gradient Descent ######### 
learning_rate <- .2
steps <- 1000
for(i in 1:steps){
  # initialize weights, runs it through activation function, calculate y_hat
  Z_2 <- X %*% W_1
  A_2 <- sigmoid(Z_2)
  Z_3 <- A_2 %*% W_2
  Y_hat <- sigmoid(Z_3)
  # find gradient
  delta_3 <- (-(Y - Y_hat) * sigmoidprime(Z_3))
  djdw2 <- t(A_2) %*% delta_3
  delta_2 <- delta_3 %*% t(W_2) * sigmoidprime(Z_2)
  djdw1 <- t(X) %*% delta_2
  # update weights
  W_1 <- W_1 - learning_rate * djdw1
  W_2 <- W_2 - learning_rate * djdw2
}

predicted <- c()
actual <- c()
for(i in 1:nrow(Y)){
  predicted[i] <- which(Y_hat[i, ] == max(Y_hat[i, ]))
  actual[i] <- which(Y[i, ] == max(Y[i, ]))
}

mean(predicted == actual) # accuracy on training data



######### Use optimized weights on test data ######### 
Z_2_test <- test %*% W_1
A_2_test <- sigmoid(Z_2_test)
Z_3_test <- A_2_test %*% W_2
Y_hat_test <- sigmoid(Z_3_test)

predicted <- c()
actual <- c()
for(i in 1:nrow(test)){
  predicted[i] <- which(Y_hat_test[i, ] == max(Y_hat_test[i, ]))
  actual[i] <- which(Y_test[i, ] == max(Y_test[i, ]))
}
mean(predicted == actual)


######### Using the pre-existing package ######### 
train <- iris[rows, ]
irismodel <- nnet(Species ~ Sepal.Length + Sepal.Width + Petal.Length + 
                    Petal.Width, size = 3, data = train)

plotnet(irismodel)
predict <- predict(irismodel, iris[-rows,])