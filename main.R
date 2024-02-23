library(readxl)
library(tidyr)
library(ggplot2)
library(data.table)

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
theta_hat <- solve(t(X) %*% X) %*% t(X) %*% yt

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
F_N_10 <- F_N
h_N_10 <- h_N

# 4.4 & 4.5 

# want to save onestep predictions in dataframe (for lambda = 0.9):
onestep_lamb_09  <- NA
sixstep_lamb_09  <- NA
twlvstep_lamb_09  <- NA
data <- data.frame(x = x, y = yt)
lambda <- 0.9
N <- length(yt)
naive <- NA
for (i in 11:N) {
  F_N <- F_N + lambda^(i - 1) * f(-(i - 1)) %*% t(f(-(i - 1)))
  h_N <- lambda * Linv %*% h_N + f(0) * yt[i]
  theta_N <- solve(F_N) %*% h_N
  
  #yhat_N <- t(f(-(i - 1):i + 12)) %*% theta_N
  onestep_lamb_09[i + 1] <- t(f(1)) %*% theta_N
  sixstep_lamb_09[i + 6] <- t(f(6)) %*% theta_N
  twlvstep_lamb_09[i + 12] <- t(f(12)) %*% theta_N
  naive[i+1] <- yt[i]
}

data_onestep <- data.frame(x = seq(-(n - 1), 1, 1), y = onestep_lamb_09)
data_sixstep <- data.frame(x = seq(-(n - 1), 6, 1), y = sixstep_lamb_09)
data_twlvstep <- data.frame(x = seq(-(n - 1), 12, 1), y = twlvstep_lamb_09)

colors = c("One-month step" = "blue",
           "Six-month step" = "green", 
           "Twelve-month step" = "red")
plot_N <- ggplot(data, aes(x = x, y = yt)) +
  geom_point() + 
  geom_point(data = data_onestep, aes(y = onestep_lamb_09, col = "One-month step"), size = 3) +
  geom_point(data = data_sixstep, aes(y = sixstep_lamb_09, col = "Six-month step"), size = 3) +
  geom_point(data = data_twlvstep, aes(y = twlvstep_lamb_09, col = "Twelve-month step"), size = 3) +
  scale_color_manual(values = colors) +
  xlab("Date") + ylab("Number of vehicles in total") + 
  labs(title = "One-month, six-month and twelve-step ahead predictions")
print(plot_N)

data_naive <- data.frame(x=seq(-(n-1),1,1), y=naive)
colors = c("Naive" = "green", "One-month step"="blue")
plot_N <- ggplot(data, aes(x = x, y = yt)) +
  geom_point() +
  geom_point(data = data_naive, aes(y=naive, col= "Naive"), size=3)  +
  geom_point(data = data_onestep, aes(y = onestep_lamb_09, col = "One-month step"), size = 3) +
  scale_color_manual(values = colors) +
  xlab("Date") + ylab("Number of vehicles in total") + 
  labs(title = "Comparision between naive persistence and one-step prediction")
print(plot_N)


# 4.6 & 4.7 & 4.8 & 4.9 

lamdas <- seq(0.55, 0.95, 0.01) 
rsmd_onestep <- vector("numeric", length(lamdas))
rsmd_sixstep <- vector("numeric", length(lamdas))
rsmd_twlvstep <- vector("numeric", length(lamdas))

data <- data.frame(x = x, y = yt)
for (j in seq_along(lamdas)) {
  lambda <- lamdas[j]
  N <- length(yt)
  i <- 1
  F_N <- (lambda^0) * f(0) %*% t(f(0))
  h_N <- (lambda^0) * f(0) * yt[i]
  onestep <- NA
  sixstep  <- NA
  twlvstep  <- NA
  # iteration
  for (i in 2:N) {
    F_N <- F_N + lambda^(i - 1) * f(-(i - 1)) %*% t(f(-(i - 1)))
    h_N <- lambda * Linv %*% h_N + f(0) * yt[i]
    if (i > 10) {
      theta_N <- solve(F_N) %*% h_N
      
      onestep[i + 1] <- t(f(1)) %*% theta_N
      sixstep[i + 6] <- t(f(6)) %*% theta_N
      twlvstep[i + 12] <- t(f(12)) %*% theta_N
    }
  }
  # calculate root-mean-square error
  rsmd_onestep[j] <- sqrt(mean((onestep[11:N] - yt[11:N])^2, na.rm = TRUE))
  rsmd_sixstep[j] <- sqrt(mean((sixstep[11:N] - yt[11:N])^2, na.rm = TRUE))
  rsmd_twlvstep[j] <- sqrt(mean((twlvstep[11:N] - yt[11:N])^2, na.rm = TRUE))
}

rmsd_data <- data.frame(
  lambda = lamdas,
  OneStep = rsmd_onestep,
  SixStep = rsmd_sixstep,
  TwelveStep = rsmd_twlvstep
)
long_rmsd_data <- pivot_longer(rmsd_data, cols = -lambda, names_to = "Horizon", values_to = "RMSD")
ggplot(long_rmsd_data, aes(x = lambda, y = RMSD, color = Horizon, group = Horizon)) +
  geom_line() +
  geom_point() +
  labs(x = "Lambda", y = "RMSD", title = "RMSD for Different Forecast Horizons") +
  theme_minimal() +
  scale_color_manual(values = c("OneStep" = "red", "SixStep" = "blue", "TwelveStep" = "green")) +
  theme(legend.title = element_blank())  # Hides the legend title if desired

optimal_lmbd_onestep = 0.55 + (which.min(rsmd_onestep) - 1) * 0.01
optimal_lmbd_sixstep = 0.55 + (which.min(rsmd_sixstep) - 1) * 0.01
optimal_lmbd_twlvstep = 0.55 + (which.min(rsmd_twlvstep) - 1) * 0.01

# 4.12
iterate_pred <- function(lambda) {
  N <- length(yt)
  i <- 1
  F_N <- (lambda^0) * f(0) %*% t(f(0))
  h_N <- (lambda^0) * f(0) * yt[i]
  theta_N <- solve(F_N) %*% h_N
  onestep <- NA
  sixstep  <- NA
  twlvstep  <- NA
  # iteration
  for (i in 2:N) {
    F_N <- F_N + lambda^(i - 1) * f(-(i - 1)) %*% t(f(-(i - 1)))
    h_N <- lambda * Linv %*% h_N + f(0) * yt[i]
    theta_N <- solve(F_N) %*% h_N
  }
  print(theta_N)
  onestep <- t(f(1)) %*% theta_N
  sixstep <- t(f(6)) %*% theta_N
  twlvstep <- t(f(12)) %*% theta_N
  return(list(onestep, sixstep, twlvstep)) 
}

# read test data 
data_test <- read_excel("test.xlsx")
ytest <- as.numeric(data_test[1, -1])
xtest <- seq(from = 2023 - 1/12, by = 1/12, length.out = length(ytest))
testdata <- data.frame(x = xtest, y = ytest)
data <- data.frame(x - xt, y = yt)

# make predictions 
xpts <- c(2023 - 1/12, 2023 + 1/12 * 5, 2023 + 1/12 * 11)
pred_optimal_lambda_for_onestep <- iterate_pred(optimal_lmbd_onestep)
pred1 = data.frame(x = xpts, y = unlist(pred_optimal_lambda_for_onestep))
pred_optimal_lambda_for_sixstep <- iterate_pred(optimal_lmbd_sixstep)
pred2 = data.frame(x = xpts, y = unlist(pred_optimal_lambda_for_sixstep))
pred_optimal_lambda_for_twlvstep <- iterate_pred(optimal_lmbd_twlvstep)
pred3 = data.frame(x = xpts, y = unlist(pred_optimal_lambda_for_twlvstep))

colors <-  c("Lambda = 0.55" = "green", 
             "Lambda = 0.79" = "purple", 
             "Lambda = 0.95" = "blue",
             "Test Data" = "red")
plot_N <- ggplot(data, aes(x = xt, y = yt)) +
  geom_point() +
  geom_point(data = testdata, aes(x = xtest, y = ytest, col = "Test Data")) +
  geom_point(data = pred1, aes(x = x, y = y, col = "Lambda = 0.55")) +
  geom_point(data = pred2, aes(x = x, y = y, col = "Lambda = 0.79")) +
  geom_point(data = pred3, aes(x = x, y = y, col = "Lambda = 0.95")) + 
  scale_color_manual(values = colors) +
  xlab("Date since 2018") + ylab("Number of Vehicles") 
print(plot_N)

