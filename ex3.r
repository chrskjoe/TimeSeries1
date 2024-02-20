library(readxl)
library(ggplot2)

file_path <- "train.xlsx"
data <- read_excel(file_path)

# Clean Data
yt <- as.numeric(data[1, -1])
xt <- seq(from = 2018, by = 1/12, length.out = length(yt))
n = length(yt)
print(xt)
print(yt)

# 1.1

plot(xt, yt, type = 'b', 
     main = "Driven cars against month since start of january",
     xlab = "Month since January 2018",
     ylab = "Motor driven vehicles"
)

# 2

# 2.1
# LS Estimator for OLS
p = 2
X <- cbind(1, xt)  # X matrix of system
# X^T @ X is full rank / linear independent
# print(X_sys)
# print(t(X_sys))
# print(t(X_sys) %*% X_sys)
# rank <- qr(t(X_sys) %*% X_sys)$rank
# print(rank)
# print(ncol(X_sys))

theta_hat <- solve(t(X) %*% X) %*% t(X) %*% yt
print(theta_hat)

# 2.2
y_hat <- X %*% theta_hat
e <- yt - y_hat
RSS <- t(e) %*% e
sigma2 <- as.numeric(RSS / (n - p))
print(sigma2) 
V <- sigma2 * solve(t(X) %*% X)
print(V)


# 2.3 
x_new <- seq(from = 2023, by = 1/12, length.out = 12)
print(x_new)
X_new <- cbind(1, x_new)  # X matrix of system
y_new <- X_new %*% theta_hat
print(y_new)

# 2.4

# 3

# 3.1 & 3.2 

n <- length(yt)
print(n)
lambda <- 0.9
SIGMA <- diag(n)
for (i in 1:n) {
  SIGMA[i, i] <- 1/lambda^(n - i)
}
print(SIGMA[50:59, 50:59])

time <- 1:n
lambda_weights <- lambda^((n-1):0)

barplot(lambda_weights, col = "grey", border = "black", # border color set to black
        main = "λ-weights vs Time", xlab = "Time", ylab = "λ-weights",
        names.arg = 1:n)

# 3.4

theta_hat <- solve(t(X) %*% solve(SIGMA) %*% X) %*% t(X) %*% solve(SIGMA) %*% yt
print(theta_hat)

# 3.5

#' Make wls forecast 
#'
#' @param y_train the training data
#' @param lambda the lambda, forgetting factor or weighted factor
#' @param time the time starts 
#' @param step the step size
#' @param forecast how many time steps to forecast
#' 
wls <- function(y_train, lambda, time, step, forecast) {
  n <- length(y_train)
  SIGMA <- diag(n)
  for (i in 1:n) {
    SIGMA[i, i] <- 1/lambda^(n - i)
  }
  
  x = seq(from = time, by = step, length.out = n)
  X = cbind(1, x)
  theta_hat <- solve(t(X) %*% solve(SIGMA) %*% X) %*% t(X) %*% solve(SIGMA) %*% y_train
  # forecast_x = seq(from = tail(x, n = 1) + step, by = step, length.out = forecast)
  forecast_x = seq(from = time, by = step, length.out = n + forecast)
  forecast_y = cbind(1, forecast_x) %*% theta_hat
  
  
  res <- list(theta_hat, forecast_y)
  return(res)
}

res09 <- wls(yt, 0.9, 2018, 1/12, 12)
res08 <- wls(yt, 0.8, 2018, 1/12, 12)
res07 <- wls(yt, 0.7, 2018, 1/12, 12)
res06 <- wls(yt, 0.6, 2018, 1/12, 12)
print(res09[[2]])
xt = seq(from = 2018, by = 1/12, length.out = length(yt))
x_forecast = seq(from = 2018, by = 1/12, length.out = length(yt) + 12)
data <- data.frame(xt = xt, yt = yt)
data_forecast_09 <- data.frame(x = x_forecast, y = res09[[2]])
data_forecast_08 <- data.frame(x = x_forecast, y = res08[[2]])
data_forecast_07 <- data.frame(x = x_forecast, y = res07[[2]])
data_forecast_06 <- data.frame(x = x_forecast, y = res06[[2]])
ggplot(data, aes(x = xt, y = yt)) +
  geom_point() +
  geom_line(data = data_forecast_09, aes(x = x, y = y), color = "red") +
  geom_line(data = data_forecast_08, aes(x = x, y = y), color = "blue") +
  geom_line(data = data_forecast_07, aes(x = x, y = y), color = "green") +
  geom_line(data = data_forecast_06, aes(x = x, y = y), color = "yellow") +
#  annotate("text", x = last(data_forecast_09$x), y = last(data_forecast_09$y), label = "0.9", hjust = -0.1) +
#  annotate("text", x = last(data_forecast_08$x), y = last(data_forecast_08$y), label = "0.8", hjust = -0.1) +
#  annotate("text", x = last(data_forecast_07$x), y = last(data_forecast_07$y), label = "0.7", hjust = -0.1) +
#  annotate("text", x = last(data_forecast_06$x), y = last(data_forecast_06$y), label = "0.6", hjust = -0.1) + 
  labs(title = "Weighted Least Squares Forecast", x = "Year", y = "Motor driven vehicles")
# geom_line(aes(x = x_forecast, y = res09[[2]]), color = "red") +
# geom_line(aes(x = x_forecast, y = res08[[2]]), color = "blue") +
# geom_line(aes(x = x_forecast, y = res07[[2]]), color = "green") +
# geom_line(aes(x = x_forecast, y = res06[[2]]), color = "yellow") +