library(lubridate)
library(dplyr)
library(splines)
library(gam)
library(ggplot2)
library(ggthemes)
library(gridExtra)
setwd("Desktop/Portfolio/Modeling Eth")
set.seed(1)

############# Import + Clean Data ############ 
  Eth <- read.csv("Updated_ETH.csv", header = T, stringsAsFactors = FALSE); View(Eth)

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
  Eth <- na.omit(Eth) # 8 days/rows didn't have a recorded volume so we omit the row/day
  
  # Date
  Eth$Date <- gsub("/", "-", Eth$Date)
  Eth$Date <- mdy(Eth$Date)
  
  Eth <- Eth %>% arrange(ymd(Eth$Date))
  Eth$Day <- seq(1, nrow(Eth))
  
  ## Only look at data after 2017-06-01
  Eth <- Eth[Eth$Date > "2017-07-01", ]

######################## Model Data - Regression ######################## 
## Step 1: Find best model by fitting diff models on train data and testing its accuracy using test data according to MSE
  n <- nrow(Eth)
  
  train <- 1: (n*0.8)
  test <- ((n*0.8)+1):nrow(Eth)
  mse <- function(y, yhat){
    return(mean((y - yhat)^2))
  }
    
  ## Single Linear Regression
  linreg <- lm(Price ~ Day, data = Eth, subset = train)
  linreg.pred <- predict(linreg, newdata = Eth[test, ])
  mse1 <- mse(Eth$Price[test], linreg.pred) 
  
  ## Polynomial Regression
  poly.2 <- lm(Price ~ poly(Day, 2), data = Eth, subset = train)
  poly.3 <- lm(Price ~ poly(Day, 3), data = Eth, subset = train)
  poly.4 <- lm(Price ~ poly(Day, 4), data = Eth, subset = train)
  poly.5 <- lm(Price ~ poly(Day, 5), data = Eth, subset = train)
  poly.6 <- lm(Price ~ poly(Day, 25), data = Eth, subset = train)
  #anova(poly.2, poly.3, poly.4, poly.5, poly.6) #As expected, deg = 25 fits data the best but it would certainly over fit so I'll just used deg = 2 and deg = 3
  poly2.pred <- predict(poly.2, newdata = Eth[test, ])
  poly3.pred <- predict(poly.3, newdata = Eth[test, ])
  mse2 <- mse(Eth$Price[test], poly2.pred) 
  mse3 <- mse(Eth$Price[test], poly3.pred) 
  
  ## Exponential Regression
  expreg <- lm(log(Price) ~ log(Day), data = Eth, subset = train)
  expreg.pred <- predict(expreg, newdata = Eth[test, ]) |> exp()
  mse4 <- mse(Eth$Price[test], expreg.pred) 
  
  ## Logarithmic Regression
  logreg <- lm(Price ~ log(Day), data = Eth, subset = train)
  logreg.pred <- predict(logreg, newdata = Eth[test, ]) 
  #pos.indeces <- which(logreg.pred > 0)
  mse5 <- mse(Eth$Price[test], logreg.pred)

  ## Smooth Spline with CV
  spline <- smooth.spline(Eth$Day[train], Eth$Price[train], cv = T)
  spline.pred <- predict(spline, data.frame('Price' = test))
  mse6 <- mse(Eth$Price[test], spline.pred$y$Price) 
  
  ## GAM with spline
  gam <- gam(Price ~ s(Day,df = 172), data = Eth, subset = train) #df comes from smooth.spline()
  gam.pred <- predict(gam, newdata = Eth[test, ])
  mse7 <- mse(Eth$Price[test], gam.pred) 
  
  
########################  Plotting Data ######################## 
  ## Single Linear Regression
  l1 <- lm(Price ~ Day, data = Eth)
  plot(Eth$Date, Eth$Price, 
       col = 1, lwd = 1, 
       type = 'l',
       xlab = "Date", ylab = "Price USD/ETH", 
       main = "Linear Regression")
  lines(Eth$Date, predict(l1), col = 2, lwd = 2.5)
  legend("topleft", inset = 0.1, legend = c("Actual", "Fitted"), fill = c(1, 2), text.font=4)

  ## Quadratic
  l2 <- lm(Price ~ poly(Day, 2), data = Eth)
  plot(Eth$Date, Eth$Price, 
       col = 1, lwd = 1, 
       type = 'l',
       xlab = "Date", ylab = "Price USD/ETH", 
       main = "Quadratic Regression")
  lines(Eth$Date, predict(l2), col = 3, lwd = 2.5)
  legend("topleft", inset = 0.1, legend = c("Actual", "Fitted"), fill = c(1,3), text.font=4)
  
  ## Cubic
  l3 <- lm(Price ~ poly(Day, 3), data = Eth)
  plot(Eth$Date, Eth$Price, 
       col = 1, lwd = 1, 
       type = 'l', 
       xlab = "Date", ylab = "Price USD/ETH",
       main = "Cubic Regression")
  lines(Eth$Date, predict(l3), col = 4, lwd = 2.5)
  legend("topleft", inset = 0.1, legend = c("Actual", "Fitted"), fill = c(1,4), text.font=4)
  
  ## Exponential
  l4 <- lm(log(Price) ~ log(Day), data = Eth)
  plot(Eth$Date, Eth$Price, 
       col = 1, lwd = 1, 
       type = 'l', 
       xlab = "Date", ylab = "Price USD/ETH",
       main = "Exponential Regression")
  lines(Eth$Date, exp(predict(l4)), col = 5, lwd = 2.5)
  legend("topleft", inset = 0.1, legend = c("Actual", "Fitted"), fill = c(1,5), text.font=4)
  
  ## Logarithmic Regression
  l5 <- lm(Price ~ log(Day), data = Eth)
  plot(Eth$Date, Eth$Price, 
       col = 1, lwd = 1, 
       type = 'l', 
       xlab = "Date", ylab = "Price USD/ETH", 
       main = "Logarithmic Regression")
  lines(Eth$Date, predict(l5), col = 6, lwd = 2.5)
  legend("topleft", inset = 0.1, legend = c("Actual", "Fitted"), fill = c(1,6), text.font=4)
  
  #GAM
  l7 <- gam(Price ~ s(Day, df = 186), data = Eth)
  plot(Eth$Date, Eth$Price, 
       col = 1, lwd = 1, 
       type = 'l',
       xlab = "Date", ylab = "Price USD/ETH",
       main = "GAM with Smooth Spline")
  lines(Eth$Date, predict(l7), col = 7, lwd = 2.5)
  legend("topleft", inset = 0.1, legend = c("Actual", "Fitted"), fill = c(1,7), text.font=4)
  
  # ALL AT ONCE
  Plot.All.Previous.Days <- ggplot(Eth, aes(Date)) +
    geom_line(aes(y = Eth$Price, color = "Actual"), linewidth = 0.5) +
    geom_line(aes(y = predict(l1), color = "Linear"), linewidth = 1.25) +
    geom_line(aes(y = predict(l2), color = "Quadratic"), linewidth = 1.25) +
    geom_line(aes(y = predict(l3), color = "Cubic"), linewidth = 1.25) +
    geom_line(aes(y = exp(predict(l4)), color = "Exponential"), linewidth = 1.25) +
    geom_line(aes(y = predict(l5), color = "Logarithmic"), linewidth = 1.25) +
    labs(x = "Date", y = "Price Eth/USD", title = "A Comparison of Ethereum Regression Models") +
    ylim(-500, 5000) +
    theme_minimal() +
    scale_color_manual(name='Models',
                       breaks=c("Actual", "Linear", "Quadratic", "Cubic", "Exponential", "Logarithmic"),
                       values=c("Actual"=1, "Linear"=2, "Quadratic"=3, "Cubic"=4, "Exponential"=5, "Logarithmic"=6))
  
  Plot.All.Previous.Days
  
  days_until_2030 <- 2210 
  all.days <- seq(1, nrow(Eth) + days_until_2030)
  Eth.alltime <- data.frame(x = all.days,
                            Linear = predict(l1, newdata = data.frame(Day = all.days)),
                            Quad = predict(l2, newdata = data.frame(Day = all.days)),
                            Cubic = predict(l3, data.frame(Day = all.days)),
                            Exponential = exp(predict(l4, data.frame(Day = all.days))),
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
  
  
## Step 2: Pick best model (ie model with lowest mse)
  models <- data.frame(Model = c("Linear", "Quad", "Cubic", "Exponential", "Logarithmic", "GAM with Spline"),
                       MSE = c(mse1, mse2, mse3, mse4, mse5, mse7)) %>% arrange(MSE) %>% format(justify = "left")
  grid.table(models)
  
## Step 3: Use best model to predict price from now (12/14/23) to 2030
  future.days <- seq(1, nrow(Eth) + days_until_2030)
  best.model <- l5
  best.pred <- predict(l5, newdata = data.frame(future.days))
   
  Plot.All.Days <- ggplot(Eth_reshaped, aes(x, y, col = Models)) + 
    geom_line(linewidth = 1) +
    labs(x = "Date", y = "Price Eth/USD", title = "A Comparison of Ethereum Regression Models' Predictions") +
    ylim(-1000, 5000) +
    theme_pander() +
    scale_x_continuous(breaks = c(0, 1460, 2920, 4380, 5000), labels = c("2016", "2020", "2024", "2028", "2030")) 
  
 Plot.All.Days
  
  x <- Eth$Price
  y <- predict(l5, newdata = data.frame(Day = all.days))
  y[nrow(Eth)+days_until_2030]
  plot(all.days, y,
       col = 4, 
       lwd = 3, 
       type = "l",
       ylim = c(0, 5000),
       xaxt = "n",
       xlab = "Date", ylab = "Price USD/ETH", main = "Logarithmic Regression")
  lines(Eth$Price)
  points(4565, y[4565], cex = 2, col = "red", pch = 20)
  points(2355, Eth$Price[nrow(Eth)], cex = 2, col = "red", pch = 20)
  text(4400, y[4565]+150, "$2751.36")
  text(2355, Eth$Price[2355]+220, "$2260.18")
  axis(1, at = c(-175, 527, 1229, 1931, 2633, 3335, 4037, 4565), 
       labels = c("2017", "2019", "2021", "2023", "2025", "2027", "2029", "2030"))
  
  


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
#Playing Around
  
  #LOESS
  l <- loess(Price ~ Day, span = 0.2, data = Eth)
  ggplot(Eth, aes(Date)) +
    geom_line(aes(y = Eth$Price), col = "black", linewidth = 1) +
    geom_line(aes(y = predict(l)), col = "blue", linewidth = 2) +
    labs(x = "Date", y = "Price Eth/USD")
  
  #LOESS ########################### idk how to predict new values
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
  

 
  
  
  
  
  
  
  
  
 
  
  
  
  
  
  
  
  
  
  
  
  
  
  