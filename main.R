library(readxl)

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
Y_hat_ols <- y_hat
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
  SIGMA[i, i] <- lambda^(n - i)
}
print(SIGMA[50:59, 50:59])

time <- 1:n
lambda_weights <- diag(SIGMA)

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
  annotate("text", x = last(data_forecast_09$x), y = last(data_forecast_09$y), label = "0.9", hjust = -0.1) +
  annotate("text", x = last(data_forecast_08$x), y = last(data_forecast_08$y), label = "0.8", hjust = -0.1) +
  annotate("text", x = last(data_forecast_07$x), y = last(data_forecast_07$y), label = "0.7", hjust = -0.1) +
  annotate("text", x = last(data_forecast_06$x), y = last(data_forecast_06$y), label = "0.6", hjust = -0.1) + 
  labs(title = "Weighted Least Squares Forecast", x = "Year", y = "Motor driven vehicles")
# geom_line(aes(x = x_forecast, y = res09[[2]]), color = "red") +
# geom_line(aes(x = x_forecast, y = res08[[2]]), color = "blue") +
# geom_line(aes(x = x_forecast, y = res07[[2]]), color = "green") +
# geom_line(aes(x = x_forecast, y = res06[[2]]), color = "yellow") +

# 4

# make new data by shifting x values to "trand model"

n <- length(yt)
print(n)

xn <- seq(-(n - 1), 0, 1)
print(xn)
xn <- cbind(1, xn)
print(xn)

# 4.1

f <- function(j) rbind(1, j)
L <- matrix(c(1.,0., 1.,1.),
            byrow=TRUE, nrow=2)
Linv <- solve(L) 

# 4.2
# lambda does not matter here
i <- 1
F_N <- (lambda^0) * f(0) %*% t(f(0))
h_N <- (lambda^0) * f(0) * yt[i]
theta_N <- solve(F_N) %*% h_N
print(F_N) 
print(h_N)

# 4.3
lambda <- 0.9
N <- 10
x <- seq(-(n - 1), 0, 1)
data <- data.frame(x = x, y = yt)
# run i == 1 before run this
for (i in 2:N) {
  F_N <- F_N + lambda^(i - 1) * f(-(i - 1)) %*% t(f(-(i - 1)))
  h_N <- lambda * Linv %*% h_N + f(0) * yt[i]
  theta_N <- solve(F_N) %*% h_N
  
  yhat_N <- t(f(-(i - 1):(N - i))) %*% theta_N
  plot_N <- ggplot(data, aes(x = x, y = yt)) +
    geom_point() + 
    geom_point(data = data[1:i,], aes(x = x[1:i], y = yhat_N[1:i]), col = "blue") + 
    geom_line(data = data[1:i,], aes(x = x[1:i], y = yhat_N[1:i]), col = "green") + 
    # geom_line(aes(x = x, y = yhat_ols), col = "red", linetype = 2) +  
    ggtitle(paste0("N = ", i))
  
  print(plot_N)
}

# 4.4 & 4.5 & 4.6

# want to save onestep predictions in dataframe (for lambda = 0.9):
onestep_lamb_09  <- NA
sixstep_lamb_09  <- NA
twlvstep_lamb_09  <- NA
data <- data.frame(x = x, y = yt)
lambda <- 0.9
N <- length(yt)
for (i in 11:N) {
  F_N <- F_N + lambda^(i - 1) * f(-(i - 1)) %*% t(f(-(i - 1)))
  h_N <- lambda * Linv %*% h_N + f(0) * yt[i]
  theta_N <- solve(F_N) %*% h_N
  
  #yhat_N <- t(f(-(i - 1):i + 12)) %*% theta_N
  onestep_lamb_09[i + 1] <- t(f(1)) %*% theta_N
  sixstep_lamb_09[i + 6] <- t(f(6)) %*% theta_N
  twlvstep_lamb_09[i + 12] <- t(f(12)) %*% theta_N
}

data_onestep <- data.frame(x = seq(-(n - 1), 1, 1), y = onestep_lamb_09)
data_sixstep <- data.frame(x = seq(-(n - 1), 6, 1), y = sixstep_lamb_09)
data_twlvstep <- data.frame(x = seq(-(n - 1), 12, 1), y = twlvstep_lamb_09)

plot_N <- ggplot(data, aes(x = x, y = yt)) +
  geom_point() + 
  geom_point(data = data_onestep, aes(y = onestep_lamb_09), col = "blue", size = 3) +
  geom_point(data = data_sixstep, aes(y = sixstep_lamb_09), col = "green", size = 3) +
  geom_point(data = data_twlvstep, aes(y = twlvstep_lamb_09), col = "red", size = 3)

print(plot_N)

