######### Initial #########
set.seed(1)
library(dplyr)
library(nnet)
library(NeuralNetTools)

df <- read.csv("diabetes.csv", header = T)
df <- df[1:100, ]
df <- df |> select(c(2:21, 1))
df$Diabetes_012 <- as.integer(as.factor(df$Diabetes_012))

######### cross validation ######### 
n <- nrow(df)
rows <- sample(1:n, 0.8 * n)

train <- as.matrix(df[rows, 1:21])
test <- as.matrix(df[-rows, 1:21])

X <- train

######### creates vector of 1/0's  ######### 
a <- matrix(0, nrow = n, ncol = 3)
a[, 1] <- df$Diabetes_012
df$Diabetes_012 <- matrix(a, nrow = n, ncol = 3)

for(i in 1:n){
  if(2 %in% df$Diabetes_012[i, ]) {
    df$Diabetes_012[i, ] <- c(0, 1, 0)
  } else if(3 %in% df$Diabetes_012[i, ]) {
    df$Diabetes_012[i, ] <- c(0, 0, 1)
  }
}

Y <- df$Diabetes_012[rows, ]
Y_test <- df$Diabetes_012[-rows, ]

######### activation function - should use reLU ######### 
sigmoid <- function(Z){
  1/(1 + exp(-Z))
}

### forward feed
W_1 <- matrix(runif(210), nrow = 21, ncol = 10)
Z_2 <- X %*% W_1
A_2 <- sigmoid(Z_2)

W_2 <- matrix(runif(60), nrow = 10, ncol = 6)
Z_3 <- A_2 %*% W_2
A_3 <- sigmoid(Z_3)

W_3 <- matrix(runif(18), nrow = 6, ncol = 3)
Z_4 <- A_3 %*% W_3
A_4 <- sigmoid(Z_4)

W_4 <- matrix(runif(9), nrow = 3, ncol = 3)
Z_5 <- A_4 %*% W_4
Y_hat <- sigmoid(Z_5)


######### Calculate Gradient - partial derivatives  ######### 
sigmoidprime <- function(z){
  exp(-z)/((1 + exp(-z))^2)
}

delta_5 <- -(Y - Y_hat) * sigmoidprime(Z_5)
djdw4 <- t(A_4) %*% delta_5

delta_4 <- delta_5 %*% t(W_4) * sigmoidprime(Z_4)
djdw3 <- t(A_3) %*% delta_4

delta_3 <- delta_4 %*% t(W_3) * sigmoidprime(Z_3)
djdw2 <- t(A_2) %*% delta_3

delta_2 <- delta_3 %*% t(W_2) * sigmoidprime(Z_2)
djdw1 <- t(X) %*% delta_2


######### Gradient Descent ######### 
learning_rate <- .5
steps <- 100
for(i in 1:steps){
  # initialize weights, runs it through activation function, calculate y_hat
  Z_2 <- X %*% W_1
  A_2 <- sigmoid(Z_2)
  Z_3 <- A_2 %*% W_2
  A_3 <- sigmoid(Z_3)
  Z_4 <- A_3 %*% W_3
  A_4 <- sigmoid(Z_4)
  Z_5 <- A_4 %*% W_4
  Y_hat <- sigmoid(Z_5)
  # find gradient
  delta_5 <- -(Y - Y_hat) * sigmoidprime(Z_5)
  djdw4 <- t(A_4) %*% delta_5
  delta_4 <- delta_5 %*% t(W_4) * sigmoidprime(Z_4)
  djdw3 <- t(A_3) %*% delta_4
  delta_3 <- delta_4 %*% t(W_3) * sigmoidprime(Z_3)
  djdw2 <- t(A_2) %*% delta_3
  delta_2 <- delta_3 %*% t(W_2) * sigmoidprime(Z_2)
  djdw1 <- t(X) %*% delta_2
  # update weights
  W_1 <- W_1 - learning_rate * djdw1
  W_2 <- W_2 - learning_rate * djdw2
  W_3 <- W_3 - learning_rate * djdw3
  W_4 <- W_4 - learning_rate * djdw4
}

predicted <- c()
actual <- c()
for(i in 1:nrow(Y)){
  predicted[i] <- which(Y_hat[i, ] == max(Y_hat[i, ]))
  actual[i] <- which(Y[i, ] == max(Y[i, ]))
}

mean(predicted == actual) # accuracy on training data


######### Use optimized weights on test data ######### 
Z_2t <- test %*% W_1
A_2t <- sigmoid(Z_2t)
Z_3t <- A_2t %*% W_2
A_3t <- sigmoid(Z_3t)
Z_4t <- A_3t %*% W_3
A_4t <- sigmoid(Z_4t)
Z_5t <- A_4t %*% W_4
Y_hat_test <- sigmoid(Z_5t)

predicted <- c()
actual <- c()
for(i in 1:nrow(test)){
  predicted[i] <- which(Y_hat_test[i, ] == max(Y_hat_test[i, ]))
  actual[i] <- which(Y_test[i, ] == max(Y_test[i, ]))
}
mean(predicted == actual)
