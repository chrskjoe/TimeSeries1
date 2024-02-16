# install.packages("readxl")

library(readxl)

file_path <- "As1Test.xlsx"
data <- read_excel(file_path)

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


# 2.1
p = 2
X_sys <- cbind(1, xt)  # X matrix of system
print(X_sys)
Theta <- solve(t(X_sys) %*% (X_sys)) %*% t(X_sys) %*% yt
print(Theta)


y_hat <- X_sys %*% Theta # The values of the function

# Calculating the variance
e_ols <- yt - y_hat   # Errors
#print(e_ols)
RSS_ols <- t(e_ols)%*%e_ols  # Squared errors
#print(RSS_ols)
sigma2_ols <- as.numeric(RSS_ols/(n-p))  # variance, sigma^2
#print(sigma2_ols)  
V <- sigma2_ols * solve(t(X_sys)%*%X_sys)  # Variance of parameters
print(V)

b0 <- (sqrt(diag(V)))[1]
b1 <- (sqrt(diag(V)))[2]
print(b0)  # sigma for intercept
print(b1)  # sigma for slope

# 2.3

x_test <- seq(from = 2020+11/12, by=1/12, length = 12)
X_test_sys <- cbind(1,x_test)

y_pred <- X_test_sys %*% Theta
