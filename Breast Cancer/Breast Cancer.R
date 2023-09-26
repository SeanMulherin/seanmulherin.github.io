setwd("/Users/seanmulherin/Desktop/Portfolio/Breast Cancer")
Breast <- read.csv("Breast.csv", header = T, stringsAsFactors = T); View(Breast)
Breast <- Breast[, 1:12]; Breast <- Breast[, -1]

library(caret) #data partitioning (train/test set)
library(MASS) #lda
library(rpart) #CART
library(class) #KNN
library(e1071) #svm and Naive Bayes
library(randomForest) #rf
library(dplyr);
library(gridExtra)

#### Partition Data
set.seed(1)
train <- createDataPartition(y = Breast$diagnosis, p = 0.7, list = F) #random generates row indexes
test <- -train
validation <- validation

####### Logistic Regression #######
logreg <- glm(diagnosis ~ ., data = Breast, subset = train, family = "binomial")
logreg.pred <- predict(logreg, Breast[test, ] , type = "response")
logreg.pred <- ifelse(logreg.pred >= 0.5, "M", "B")
err1 <- mean(logreg.pred != validation)

####### Linear Discriminant Analysis ####### 
lda <- lda(diagnosis ~., data = Breast, subset = train)
lda.pred <- predict(lda, Breast[test, ])
err2 <- mean(lda.pred$class != validation)

####### Quadratic Discriminant Analysis #######
qda <- qda(diagnosis ~., data = Breast, subset = train)
qda.pred <- predict(qda, Breast[test, ])
err3 <- mean(qda.pred$class != validation)

####### Naive Bayes #######
nb <- naiveBayes(diagnosis ~ ., data = Breast, subset = train)
nb.pred <- predict(nb, Breast[test, ])
err4 <- mean(nb.pred != validation)

####### KNN #######
train.Matrix <- cbind(radius_mean, texture_mean, perimeter_mean, area_mean, smoothness_mean, compactness_mean, concavity_mean, concave_points_mean, symmetry_mean, fractal_dimension_mean)[train, ]
test.Matrix <- cbind(radius_mean, texture_mean, perimeter_mean, area_mean, smoothness_mean, compactness_mean, concavity_mean, concave_points_mean, symmetry_mean, fractal_dimension_mean)[test, ]
knn.pred <- knn(train.Matrix, test.Matrix, Breast[train, "diagnosis"], k=1)
err5 <- mean(knn.pred != validation)

####### CART with 10-fold cv #######
train.control <- trainControl(method = "cv", number = 10, savePredictions = T)
cart <- train(diagnosis ~., data = Breast, subset = train, trControl = train.control, tuneLength=10, method = "rpart") #tuneLength controls regularization penalty, rpart invokes cart package, trControl invokes cv
cart.pred <- predict(cart, Breast[test, ])
err6 <- mean(cart.pred != validation)

####### Random Forest #######
rf1 <- train(diagnosis ~., data = Breast, subset = train, method = "rf", trControl = train.control, tuneLength = 2, preProcess = c("center", "scale")) #mtry represents the # of nodes (how complex your tree is)
rf1.pred <- predict(rf1, Breast[test, ])
err7 <- mean(rf1.pred != validation)

rf2 <- randomForest(diagnosis ~., data = Breast, subset = train) #uses the rf package instead of cv
rf2.pred <- predict(rf2, Breast[test, ])
err8 <- mean(rf2.pred != validation)

####### SVM #######
svm <- svm(diagnosis ~., data = Breast, subset = train)
svm.pred <- predict(svm, Breast[test, ])
err9 <- mean(svm.pred != validation)

################### Summarize Error Rates ################
models <- data.frame(Model = c("Logistic", "LDA", "QDA", "Naive Bayes", "KNN", "Decision Tree", "Random Forest 1", "Random Forest 2", "SVM"),
                Error = c(err1, err2, err3, err4, err5, err6, err7, err8, err9)) %>% mutate(Error = round(Error, 4)) %>% arrange(Error)
grid.table(models)

best.model <- lda(diagnosis ~., Breast)
best.pred <- predict(best.model, Breast)  
err.best <- mean(best.pred$class != Breast$diagnosis)




################### Plotting ################












