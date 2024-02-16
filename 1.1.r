# install.packages("readxl")
# install.packages("ggplot2")

library(readxl)

file_path <- "As1Train.xlsx"
data <- read_excel(file_path)

y <- as.numeric(data[1, -1])
x <- seq(from = 2018, by = 1/12, length.out = length(yt))
n = length(y)
print(x)
print(y)

# 1.1

plot(x, y, type = 'b', 
     main = "Driven cars against month since start of january",
     xlab = "Month since January 2018",
     ylab = "Motor driven vehicles"
     )


# 2.1
p = 2
X <- cbind(1, xt)  # X matrix of system
print(X)
Theta <- solve(t(X) %*% (X)) %*% t(X) %*% y
print(Theta)


y_hat <- X %*% Theta # The values of the function

# Calculating the variance
e_ols <- yt - y_hat   # Errors
#print(e_ols)
RSS_ols <- t(e_ols)%*%e_ols  # Squared errors
#print(RSS_ols)
sigma2_ols <- as.numeric(RSS_ols/(n-p))  # variance, sigma^2
#print(sigma2_ols)  
V <- sigma2_ols * solve(t(X)%*%X)  # Variance of parameters
print(V)

b0 <- (sqrt(diag(V)))[1]
b1 <- (sqrt(diag(V)))[2]
print(b0)  # sigma for intercept
print(b1)  # sigma for slope

# 2.3

x_test <- seq(from = 2022+11/12, by=1/12, length = 12)
X_test <- cbind(1,x_test)

y_pred <- X_test %*% Theta
V_pred <- sigma2_ols * (1 + (X_test %*% solve(t(X)%*%X))%*%t(X_test))
print(V_pred)

# compute "prediction intervals"
y_pred_lwr <- y_pred - 1.667*sqrt(diag(V_pred))
y_pred_upr <- y_pred + 1.667*sqrt(diag(V_pred))


library(ggplot2)

# Your existing script up to the calculation of prediction intervals...

# Create data frames for plotting
plot_data <- data.frame(month = c(x, x_test), y = c(y, rep(NA, length(x_test))), y_pred = c(rep(NA, length(x)), y_pred), y_pred_lwr = c(rep(NA, length(x)), y_pred_lwr), y_pred_upr = c(rep(NA, length(x)), y_pred_upr))

# Plotting
ggplot(plot_data, aes(x = month)) +
  geom_point(aes(y = y), color = "blue") +
  geom_line(aes(y = y), color = "blue", linetype = "dashed") +
  geom_point(aes(y = y_pred), color = "red") +
  geom_line(aes(y = y_pred), color = "red") +
  geom_ribbon(aes(ymin = y_pred_lwr, ymax = y_pred_upr), fill = "red", alpha = 0.2) +
  labs(title = "Driven Cars Forecast Beyond Given Data",
       x = "Month since January 2018",
       y = "Motor Driven Vehicles") +
  theme_minimal()


