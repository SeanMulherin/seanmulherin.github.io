#Import Data
  setwd("Desktop/Portfolio/Modeling Eth")
  Eth <- read.csv("Ethereum Historical Data.csv", header = T, stringsAsFactors = FALSE); View(Eth)

#Clean Data  
  colnames(Eth)[6] <- "Vol"; colnames(Eth)[7] <- "Change" 
  Eth$Price <- gsub(",", "", Eth$Price); Eth$Open <- gsub(",", "", Eth$Open); Eth$High <- gsub(",", "", Eth$High); Eth$Low <- gsub(",", "", Eth$Low); Eth$Change <- gsub("%", "", Eth$Change)
  Eth$Price <- as.numeric(Eth$Price); Eth$Open <- as.numeric(Eth$Open); Eth$High <- as.numeric(Eth$High); Eth$Low <- as.numeric(Eth$Low); Eth$Change <- as.numeric(Eth$Change)
  
  for(i in 1:nrow(Eth)){ #coverts volume to numeric. Inputs included entries like "12.3K", "13.1B", and "123.6M"
    if(grepl("K", Eth$Vol[i])){
      Eth$Vol[i] <- gsub("K", "", Eth$Vol[i])
      Eth$Vol[i] <- as.numeric(Eth$Vol[i])*1000
    } else if(grepl("M", Eth$Vol[i])){
      Eth$Vol[i] <- gsub("M", "", Eth$Vol[i])
      Eth$Vol[i] <- as.numeric(Eth$Vol)*1000000
    } else if(grepl("B", Eth$Vol[i])){
      Eth$Vol[i] <- gsub("B", "", Eth$Vol[i])
      Eth$Vol[i] <- as.numeric(Eth$Vol)*1000000000
    }
  }
  Eth$Vol <- as.numeric(Eth$Vol)
  Eth <- na.omit(Eth) #8 days/rows didn't have a recorded volume so we omit the row/day
  
  library(lubridate)
  Eth$Date <- gsub("/", "-", Eth$Date)
  Eth$Date <- mdy(Eth$Date)
  
  #Arrange in descending order
  library(dplyr)
  Eth <- Eth %>% arrange(ymd(Eth$Date))
  Eth$Day <- seq(1, nrow(Eth))

############ Model Data - Regression
#Step 1: Find best model by fitting diff models on train data and testing its accuracy using test data according to MSE
  set.seed(1)
  train <- seq(1, 2197) #First 80% of days covered in the data
  test <- seq(2198, nrow(Eth))
  mse <- function(y, yhat){
    return(mean((y - yhat)^2))
  }
    
  #Single Linear Regression
  linreg <- lm(Price ~ Day, data = Eth, subset = train)
  linreg.pred <- predict(linreg, newdata = Eth[test, ])
  mse1 <- mse(Eth$Price[test], linreg.pred) #mse1 = 849668.5
  
  #Polynomial Regression
  poly.2 <- lm(Price ~ poly(Day, 2), data = Eth)
  poly.3 <- lm(Price ~ poly(Day, 3), data = Eth)
  poly.4 <- lm(Price ~ poly(Day, 4), data = Eth)
  poly.5 <- lm(Price ~ poly(Day, 5), data = Eth)
  poly.6 <- lm(Price ~ poly(Day, 25), data = Eth)
  #anova(poly.2, poly.3, poly.4, poly.5, poly.6, poly.7, poly.6) #As expected, deg = 25 fits data the best but it would certainly overfit so I'll just used deg = 2 and deg = 3
  poly.2 <- lm(Price ~ poly(Day, 2), data = Eth, subset = train)
  poly.3 <- lm(Price ~ poly(Day, 3), data = Eth, subset = train)
  poly2.pred <- predict(poly.2, newdata = Eth[test, ])
  poly3.pred <- predict(poly.3, newdata = Eth[test, ])
  mse2 <- mse(Eth$Price[test], poly2.pred) #mse2 = 10442199
  mse3 <- mse(Eth$Price[test], poly3.pred) #mse3 = 46369554
  
  #Exponential Regression
  expreg <- lm(log(Price) ~ Day, data = Eth, subset = train)
  expreg.pred <- predict(expreg, newdata = Eth[test, ])
  mse4 <- mse(Eth$Price[test], expreg.pred) #mse4 = 3202453
  
  #Logarithmic Regression
  logreg <- lm(Price ~ log(Day), data = Eth, subset = train)
  logreg.pred <- predict(logreg, newdata = Eth[test, ]) 
  #pos.indeces <- which(logreg.pred > 0)
  mse5 <- mse(Eth$Price[test], logreg.pred) #mse5 = 367581.7

  #Smooth Spline with CV
  library(splines)
  spline <- smooth.spline(Eth$Day[train], Eth$Price[train], cv = T)
  spline.pred <- predict(spline, data.frame(Price = test))
  mse6 <- mse(Eth$Price[test], spline.pred$y$Price) #mse6 = 766964584
  
  #GAM with spline
  set.seed(1)
  library(gam)
  gam <- gam(Price ~ s(Day,df = 186), data = Eth, subset = train) #df = 186 comes from smooth.spline()
  gam.pred <- predict(gam, newdata = Eth[test, ])
  mse7 <- mse(Eth$Price[test], gam.pred) #mse7 = 1428626194
  
  
############ Plotting Data
  library(ggplot2); library(ggthemes)
  #Actual Eth Price Data
  ggplot(Eth, aes(Date, Price)) +
    geom_point() +
    geom_smooth() +
    theme_linedraw()
  
  #Single Linear Regression
  l1 <- lm(Price ~ Day, data = Eth)
  plot(Eth$Date, Eth$Price, col = 1, lwd = 1, type = 'l', xlab = "Date", ylab = "Price USD/ETH", main = "Linear Regression")
  lines(Eth$Date, predict(l1), col = 2, lwd = 2.5)
  legend("topleft", inset = 0.1, legend = c("Actual", "Fitted"), fill = c(1, 2), text.font=4)

  #Quadratic
  l2 <- lm(Price ~ poly(Day, 2), data = Eth)
  plot(Eth$Date, Eth$Price, col = 1, lwd = 1, type = 'l', xlab = "Date", ylab = "Price USD/ETH", main = "Quadratic Regression")
  lines(Eth$Date, predict(l2), col = 3, lwd = 2.5)
  legend("topleft", inset = 0.1, legend = c("Actual", "Fitted"), fill = c(1,3), text.font=4)
  
  #Cubic
  l3 <- lm(Price ~ poly(Day, 3), data = Eth)
  plot(Eth$Date, Eth$Price, col = 1, lwd = 1, type = 'l', xlab = "Date", ylab = "Price USD/ETH", main = "Cubic Regression")
  lines(Eth$Date, predict(l3), col = 4, lwd = 2.5)
  legend("topleft", inset = 0.1, legend = c("Actual", "Fitted"), fill = c(1,4), text.font=4)
  
  #Exponential
  l4 <- lm(log(Price) ~ Day, data = Eth)
  plot(Eth$Date, Eth$Price, col = 1, lwd = 1, type = 'l', xlab = "Date", ylab = "Price USD/ETH", main = "Exponential Regression")
  lines(Eth$Date, predict(l4), col = 5, lwd = 2.5)
  legend("topleft", inset = 0.1, legend = c("Actual", "Fitted"), fill = c(1,5), text.font=4)
  
  #Logarithmic Regression
  l5 <- lm(Price ~ log(Day), data = Eth)
  plot(Eth$Date, Eth$Price, col = 1, lwd = 1, type = 'l', xlab = "Date", ylab = "Price USD/ETH", main = "Logarithmic Regression")
  lines(Eth$Date, predict(l5), col = 6, lwd = 2.5)
  legend("topleft", inset = 0.1, legend = c("Actual", "Fitted"), fill = c(1,6), text.font=4)
  
  #GAM
  l7 <- gam(Price ~ s(Day, df = 186), data = Eth)
  plot(Eth$Date, Eth$Price, col = 1, lwd = 1, type = 'l', xlab = "Date", ylab = "Price USD/ETH", main = "GAM with Smooth Spline")
  lines(Eth$Date, predict(l7), col = 7, lwd = 2.5)
  legend("topleft", inset = 0.1, legend = c("Actual", "Fitted"), fill = c(1,7), text.font=4)
  
  # ALL AT ONCE
  Plot.All.Previous.Days <- ggplot(Eth, aes(Date)) +
    geom_line(aes(y = Eth$Price, color = "Actual"), linewidth = 0.5) +
    geom_line(aes(y = predict(l1), color = "Quadratic"), linewidth = 1.25) +
    geom_line(aes(y = predict(l2), color = "Cubic"), linewidth = 1.25) +
    geom_line(aes(y = predict(l3), color = "Exponential"), linewidth = 1.25) +
    geom_line(aes(y = predict(l4), color = "Logarithmic"), linewidth = 1.25) +
    geom_line(aes(y = predict(l5), color = "GAM"), linewidth = 1.25) +
    labs(x = "Date", y = "Price Eth/USD", title = "A Comparison of Ethereum Regression Models") +
    ylim(-500, 5000) +
    theme_minimal() +
    scale_color_manual(name='Models',
                       breaks=c("Actual", "Linear", "Quadratic", "Cubic", "Exponential", "Logarithmic"),
                       values=c("Actual"=1, "Quadratic"=2, "Cubic"=3, "Exponential"=4, "Logarithmic"=5))
  
  
  
  l1 <- lm(Price ~ Day, data = Eth)
  l2 <- lm(Price ~ poly(Day, 2), data = Eth)
  l3 <- lm(Price ~ poly(Day, 3), data = Eth)
  l4 <- lm(log(Price) ~ Day, data = Eth)
  l5 <- lm(Price ~ log(Day), data = Eth)
  l7 <- gam(Price ~ s(Day, df = 186), data = Eth)
  
  all.days <- seq(1, nrow(Eth)+2294)
  Eth.alltime <- data.frame(x = all.days,
                            Linear = predict(l1, newdata = data.frame(Day = all.days)),
                            Quad = predict(l2, newdata = data.frame(Day = all.days)),
                            Cubic = predict(l3, data.frame(Day = all.days)),
                            Exponential = predict(l4, data.frame(Day = all.days)),
                            Logarithmic = predict(l5, newdata = data.frame(Day = all.days)),
                            GAM = predict(l7, newdata = data.frame(Day = all.days)))
  
  Eth_reshaped <- data.frame(x = Eth.alltime$x,                           
                            y = c(Eth.alltime$Linear, Eth.alltime$Quad, Eth.alltime$Cubic, Eth.alltime$Exponential, Eth.alltime$Logarithmic, Eth.alltime$GAM),
                            Models = c(rep("Linear", nrow(Eth.alltime)),
                                      rep("Quad", nrow(Eth.alltime)),
                                      rep("Cubic", nrow(Eth.alltime)),
                                      rep("Exponential", nrow(Eth.alltime)),
                                      rep("Logarithmic", nrow(Eth.alltime)),
                                      rep("GAM", nrow(Eth.alltime))))
  
  
  
#Step 2: Pick best model (ie model with lowest mse)
  models <- data.frame(Model = c("Linear", "Quad", "Cubic", "Exponential", "Logarithmic", "GAM with Spline"),
                       MSE = c(mse1, mse2, mse3, mse4, mse5, mse7)) %>% arrange(MSE) %>% format(justify = "left")
  library(gridExtra)
  grid.table(models)
  
#Step 3: Use best model to predict price from now (9/21/23) to 2030
  future.days <- seq(1, nrow(Eth)+2294)
  best.model <- l5
  best.pred <- predict(l5, newdata = data.frame(future.days))
   
  Plot.All.Days <- ggplot(Eth_reshaped, aes(x, y, col = Models)) + 
    geom_line(size = 1.15) +
    labs(x = "Date", y = "Price Eth/USD", title = "A Comparison of Ethereum Regression Models' Predictions") +
    ylim(-1000, 5000) +
    theme_pander() +
    scale_color_manual(values=c(2,3, 4, 5, 6, 7)) +
    scale_x_continuous(breaks = c(0, 1460, 2920, 4380, 5000), labels = c("2016", "2020", "2024", "2028", "2030"))
 
  
  x <- Eth$Price
  l5 <- lm(Price ~ log(Day), data = Eth)
  y <- predict(l5, newdata = data.frame(Day = all.days))
  plot(all.days, y,
       col = 4, 
       lwd = 3, 
       type = "l",
       xlab = "Date", ylab = "Price USD/ETH", main = "Logarithmic Regression",
       ylim = c(0, 2000),
       xaxt = "n")
  points(5000, y[5000], cex = 2, col = 1, pch = 20)
  text(4000, y[5000], "$1936.31")
  axis(1, at = c(0, 730, 1460, 2190, 2920, 3650, 4380, 5000), labels = c("2016", "2018", "2020", "2022", "2024", "2026", "2028", "2030"))
  
  a <- Eth$Price
  
  


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
#Playing Around
  
  #LOESS
  l <- loess(Price ~ Day, span = 0.2, data = Eth)
  ggplot(Eth, aes(Date)) +
    geom_line(aes(y = Eth$Price), col = "black", linewidth = 1) +
    geom_line(aes(y = predict(l)), col = "blue", linewidth = 2) +
    labs(x = "Date", y = "Price Eth/USD")
  
  #LOESS ########################### idk how to preduct new values
  loess <- loess(Price ~ Day, span = 0.2, data = Eth, subset = train)
  loess.pred <- predict(loess, Eth[test, ], control=loess.control(surface="direct"))
  
  #Smooth Spline with cv
  l6 <- smooth.spline(Eth$Day, Eth$Price, cv = T)
  ggplot(Eth, aes(Date)) +
    geom_line(aes(y = Eth$Price), col = "black", linewidth = 1) +
    geom_line(aes(y = predict(l6)$y), col = "blue", linewidth = 2) +
    labs(x = "Date", y = "Price Eth/USD")
  
  
  ggplot(Eth.alltime, aes(Day)) +
    geom_line(aes(y = Eth$Price), col = "black") +
    geom_line(aes(y = Linear), col = 2) +
    geom_line(aes(y = Poly), col = 3) +
    geom_line(aes(y = Smooth), col = 4) +
    geom_line(aes(y = GAM), col = 6) +
    geom_line(aes(y = Log), col = 7) +
    labs(x = "Date", y = "Price Eth/USD") +
    ylim(-1000, 5000)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
 
  
  
  
  
  
  
  
  
  
  
  
  
  
  