######### Initial #########
set.seed(1)
library(dplyr)
library(nnet)
library(NeuralNetTools)
#library(mltools)

df <- read.csv("diabetes.csv", header = T)
df <- df[1:1000, ]
#df <- df |> select(c(HighBP, HighChol, CholCheck, BMI, Stroke, HeartDiseaseorAttack,
 #                     HvyAlcoholConsump, GenHlth, PhysHlth, Sex, Age, Income, Diabetes_012))
df$BMI <- ifelse(df$BMI <= 27, 0, 1)
df$GenHlth <- ifelse(df$GenHlth <= 3, 0, 1)
df$PhysHlth <- ifelse(df$PhysHlth <= 15, 0, 1)
df$Age <- ifelse(df$Age <= 8, 0, 1)
df$Income <- ifelse(df$Income <= 7, 0, 1)
df$MentHlth <- ifelse(df$MentHlth <= 15, 0, 1)
df$Education <- ifelse(df$Education <= 3, 0, 1)
df$Diabetes_012 <- as.integer(as.factor(df$Diabetes_012))
df$Diabetes_012 <-  ifelse(df$Diabetes_012 < 2, 0, 1)
df <- df |> select(c(2:22, 1))



######### cross validation ######### 
n <- nrow(df)
rows <- sample(1:n, 0.8 * n)

train <- as.matrix(df[rows, 1:21])
test <- as.matrix(df[-rows, 1:21])

X <- train

######### creates vector of 1/0's  ######### 
a <- matrix(0, nrow = n, ncol = 2)
a[, 1] <- df$Diabetes_012
df$Diabetes_012 <- matrix(a, nrow = n, ncol = 2)

for(i in 1:n){
  if(df$Diabetes_012[i, 1] == 0) {
    df$Diabetes_012[i, ] <- c(1, 0)
  } else {
    df$Diabetes_012[i, ] <- c(0, 1)
  }
}

Y <- df$Diabetes_012[rows, ]
Y_test <- df$Diabetes_012[-rows, ]

######### activation function - should use reLU ######### 
sigmoid <- function(Z){
  1/(1 + exp(-Z))
}

### forward feed
W_1 <- matrix(runif(42), nrow = 21, ncol = 2)
W_2 <- matrix(runif(4), nrow = 2, ncol = 2)
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
learning_rate = 0.01
steps = 100000
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
table(predicted)
cbind(Y_hat, Y)




######### Use optimized weights on test data ######### 
Z_2t <- test[1, ] %*% W_1
A_2t <- sigmoid(Z_2t)
Z_3t <- A_2t %*% W_2
Y_hat_test <- sigmoid(Z_3t)

predicted <- c()
actual <- c()
for(i in 1:nrow(test)){
  predicted[i] <- which(Y_hat_test[i, ] == max(Y_hat_test[i, ]))
  actual[i] <- which(Y_test[i, ] == max(Y_test[i, ]))
}
mean(predicted == actual)




