


library(dplyr)

options(scipen = 999)
data <- read_csv("~/train.csv")
data <- as.data.frame(data[, c(1:81)])


prices <- as.data.frame(data$SalePrice)
data <-
  data[, c(
    "OverallQual",
    "GrLivArea",
    "GarageCars",
    "GarageArea",
    "TotalBsmtSF",
    "1stFlrSF",
    "FullBath",
    "TotRmsAbvGrd",
    "YearBuilt",
    "YearRemodAdd"
  )]




#### Normalization

normalize <- function(df) {
  for (i in 1:length(colnames(df)))
  {
    if (class(df[, i]) == "numeric" || class(df[, i]) == "integer")
      
    {
      df[, i] <- as.vector((df[, i] - mean(df[, i])) / sd(df[, i]))
    }
  }
  as.data.frame(df)
}


normalizeddata <- normalize(data)
normalizedprices <- as.data.frame(normalize(prices))


lm(`data$SalePrice` ~ . , cbind(normalizedprices, normalizeddata))


### gradient descent  ###

grad_des <- function(x, y, alpha, num_iters) {
  # squared error cost function
  cost <- function(X, y, theta) {
    sum((X %*% theta - y) ^ 2) / (2 * length(y))
  }
  
  
  x <- as.matrix(x)
  y <- as.matrix(y)
  
  # keep history
  cost_history <- double(num_iters)
  theta_history <- list(num_iters)
  
  # initialize coefficients
  theta <- matrix(0, nrow = ncol(x) + 1)
  
  # add a column of 1's for the intercept coefficient
  X <- cbind(1, x)
  
  
  for (i in 1:num_iters) {
    error <- (X %*% theta - y)
    delta <- t(X) %*% error / length(y)
    theta <- theta - alpha * delta
    cost_history[i] <- cost(X, y, theta)
    theta_history[[i]] <- theta
  }
  plot(
    cost_history,
    type = 'line',
    col = 'blue',
    lwd = 2,
    main = 'Cost function',
    ylab = 'cost',
    xlab = paste('Iterations', " LR is ", alpha)
  )
  print(theta)
  
  
}

# Application


grad_des(normalizeddata, normalizedprices, 0.03, 150)
grad_des(normalizeddata, normalizedprices, 0.3, 150)
grad_des(normalizeddata, normalizedprices, 0.003, 150)
grad_des(normalizeddata, normalizedprices, 0.0003, 150)


##Split and apply

evaluate <- function (x, y, splitrate) {
  n <- nrow(x)
  
  lines <- 1:n
  mae <- as.numeric()
  
  for (i in c(1:5)) {
    train <- sample(lines, splitrate * n)
    test  <- setdiff(lines, train)
    
    
    train_x <- x[train,]
    train_y <- y[train,]
    
    test_x <- x[test,]
    test_y <- y[test,]
    
    
    coefs <- grad_des(train_x, train_y , 0.03, 20)
    
    preds <-
      as.matrix(test_x) %*%  as.matrix(coefs[c(2:11),]) + coefs[1,]
    
    print(mean(abs(test_y - preds)))
    mae <- rbind(mae, mean(abs(test_y - preds)))
    
  }
  
  print("MAE is")
  print(mean(mae))
  
}





evaluate(normalizeddata, normalizedprices, 0.80)
