

### Example for TSA Lecture 2
# 1- Linear regression with matrix notation
# 2- forecast (prediction)
# 3- Global trend model notation
# 4- Global trend model iterative updates
# 5- WLS - example with correlated epsilons

# load some packages:
library(fpp2)
library(dplyr)
library(tidyverse)


# load data:
# The "austa" data = Total international visitors to Australia (in millions) 
# from 1980 to 2015. Yearly data (ie one datapoint per year). In total 36 datapoints.

head(austa)
# austa is a "time-series" object, but we will convert to a dataframe

austa_data <- data.frame(int_visit=as.matrix(austa), year=as.matrix(time(austa)))
rownames(austa_data) <- NULL
head(austa_data)

# Now we split the data into test and train:
austa_train <- austa_data %>% filter(., year <= 2005)
austa_test <- austa_data %>% filter(., year > 2005)


### Plot train and test data
ggplot(austa_train, aes(x=year, y=int_visit)) +
  geom_point() + 
  geom_point(data=austa_test, col="blue")


# 1- Linear regression with matrix notation

# we will fit a linear model: Y_i = beta_0 + beta_1 * time_i + epsilon_i
# so we have two parameters: beta_0 and beta_1
nparams <- 2

# we also save the number of observations:
n <- length(austa_train$year)


# X is the "design matrix"
X <- cbind(1, austa_train$year)
print(X)

# y is vector with observations:
y <- cbind(austa_train$int_visit)
print(y)

# to estimate parameters we solve the "normal equations":
(OLS <- solve(t(X)%*%X)%*%t(X)%*%y)
# these are the parameter estimates!
theta_0 <- OLS[1]
theta_1 <- OLS[2]

# now we compute y_hat values for all observations:
yhat_ols <- X%*%OLS
print(yhat_ols)

# plot:
ggplot(austa_train, aes(x=year, y=int_visit)) +
  geom_point() + 
  geom_line(aes(y=yhat_ols), col="red", size=.5) 


# we will now calculate the standard errors on the parameters beta_0 and beta_1:

# first compute residuals:
e_ols <- y - yhat_ols
print(e_ols)

# calculate sum of squared residuals:
RSS_ols <- t(e_ols)%*%e_ols
print(RSS_ols)

# calculate sigma^2:
sigma2_ols <- as.numeric(RSS_ols/(n - nparams)) # = "Residual variance"
sqrt(sigma2_ols) # = "Residual standard error"

# calculate variance-covariance matrix of _parameters_:
V_ols <- sigma2_ols * solve(t(X) %*% X)
print(V_ols)

# the variances of the parameters are the values in the diagonal:
diag(V_ols)
# and the standard errors are given by:
(sqrt(diag(V_ols))) 

se_theta_0 <- (sqrt(diag(V_ols)))[1]
se_theta_1 <- (sqrt(diag(V_ols)))[2]

# now we have both point estimates and standard errors:
# intercept:
theta_0
se_theta_0
# slope:
theta_1
se_theta_1


# 2- forecast (prediction)

# now we use the model for predictions on future timepoints
# we use the timepoints from the testdata:
Xtest <- cbind(1, austa_test$year)
print(Xtest)

# compute predictions 
y_pred <- Xtest%*%OLS
print(y_pred)

# compute prediction variance-covariance matrix:
Vmatrix_pred <- sigma2_ols*(1+(Xtest%*%solve(t(X)%*%X))%*%t(Xtest))

# the variances of individual predictions are in the diagonal of the matrix above

# compute "prediction intervals"
y_pred_lwr <- y_pred - 1.96*sqrt(diag(Vmatrix_pred))
y_pred_upr <- y_pred + 1.96*sqrt(diag(Vmatrix_pred))


# plot forecast:
ggplot(austa_train, aes(x=year, y=int_visit)) +
  geom_point() + 
  geom_line(aes(y=yhat_ols), col="red", size=.5) +
  geom_point(data=austa_test, aes(x=year,y=y_pred), col="red", size=.5) +
  geom_ribbon(data=austa_test, aes(x=year,ymin=y_pred_lwr, ymax=y_pred_upr), inherit.aes=FALSE, alpha=0.2, fill="red")

# plot WITH test data:
ggplot(austa_train, aes(x=year, y=int_visit)) +
  geom_point() + 
  geom_line(aes(y=yhat_ols), col="red", size=.5) +
  geom_point(data=austa_test, aes(x=year,y=y_pred), col="red", size=.5) +
  geom_ribbon(data=austa_test, aes(x=year,ymin=y_pred_lwr, ymax=y_pred_upr), inherit.aes=FALSE, alpha=0.2, fill="red") +
  geom_point(data=austa_test, aes(x=year,y=int_visit), col="blue", size=.5)



# 3- Global trend model notation

# total number of observations (in train data) is 26:
print(n)

# we will now make a global trend model from the reference point at timepoint 26

# instead of our original x-values, which were:
austa_train$year

# we will now make the new x-values:
austa_train$x_26 <- seq(-(n-1), 0, 1)
print(austa_train$x_26)

# make design matrix for the trend model 
X_N <- cbind(1, austa_train$x_26)
print(X_N)

# compute F_N (= F_26)
F_N <- t(X_N)%*%X_N

# compute h_N (= h_26)
h_N <- t(X_N)%*%y

# estimate of parameters at timepoint 26:
theta_N <- solve(F_N)%*%h_N
print(theta_N)
# [1,] 5.115330
# [2,] 0.188649

# these estimates give the intercept at timepoint 26 and slope (in units of 1 per timepoint, i.e., one per year)

# lets compute yhat values:
yhat_N <- X_N%*%theta_N

# plot:
ggplot(austa_train, aes(x=x_26, y=int_visit)) +
  geom_point() + 
  geom_line(aes(y=yhat_N), col="red", size=.5) 

# its the same model - but scale of x-axis is changed, so parameters correspond to latest timepoint

# predictions (forecast) with trend model notation:
f <- function(j) rbind(1, j)
f(0)
f(1)
f(2)

# now make predictions for next 10 timepoints:
y_pred_N <- t(f(1:10)) %*% theta_N
 
# also calculate prediction intervals:
e_N <- y - yhat_N
RSS <-  t(e_N)%*%e_N
sigma_2 <- as.numeric(RSS/(n-nparams))

y_pred_N_lwr <- y_pred_N - 1.96*sqrt(sigma_2)*sqrt(1+diag(t(f(1:10))%*%solve(F_N)%*%f(1:10)))
y_pred_N_upr <- y_pred_N + 1.96*sqrt(sigma_2)*sqrt(1+diag(t(f(1:10))%*%solve(F_N)%*%f(1:10)))

# save predictions in dataframe in order to plot:
austa_test$x_26 <- 1:10
austa_test$y_pred_N <- y_pred_N

# plot:
ggplot(austa_train, aes(x=x_26, y=int_visit)) +
  geom_point() + 
  geom_line(aes(y=yhat_N), col="red", size=.5) +
  geom_point(data=austa_test, aes(x=x_26, y=y_pred_N), col="red", size=.5) + 
  geom_ribbon(data=austa_test, aes(x=x_26,ymin=y_pred_N_lwr, ymax=y_pred_N_upr), inherit.aes=FALSE, alpha=0.2, fill="red") +
  geom_point(data=austa_test, aes(x=x_26,y=int_visit), col="blue", size=.5)



# 4- Global trend model iterative updates

L <- matrix(c(1.,0., 1.,1.),
            byrow=TRUE, nrow=2)
print(L)
Linv <- solve(L)

# now we get the next observation (number 27) and update the data
austa_train_27 <- rbind(austa_train, austa_test[1,1:3])
austa_train_27$x_27 <- seq(-26,0, 1)

austa_test_27 <- austa_test[2:10, 1:3]
austa_test_27$x_27 <- seq(1,9, 1)
# add an extra line (with no value in int_visit)
austa_test_27 <- rbind(austa_test_27, c(NA, 2016,11,10))


# we UPDATE our model
ynew <- austa_train_27$int_visit[27]

F_N_27 <- F_N + f(-n) %*% t(f(-n))  
h_N_27 <- Linv %*% h_N + f(0)*ynew
theta_N_27 <- solve(F_N_27)%*%h_N_27

print(theta_N_27)
# 5.2702522
# 0.1867399
# slight change in parameters

# now make predictions for next 10 timepoints:
y_pred_N_27 <- t(f(1:10)) %*% theta_N_27


# also calculate prediction intervals:
X_N_27 <- cbind(1, austa_train_27$x_27)
y_hat_27 <- X_N_27 %*% theta_N_27
e_N_27 <- austa_train_27$int_visit - y_hat_27
RSS_27 <-  t(e_N_27)%*%e_N_27
sigma_2_27 <- as.numeric(RSS_27/(27-nparams))

y_pred_27_lwr <- y_pred_N_27 - 1.96*sqrt(sigma_2_27)*sqrt(1+diag(t(f(1:10))%*%solve(F_N)%*%f(1:10)))
y_pred_27_upr <- y_pred_N_27 + 1.96*sqrt(sigma_2_27)*sqrt(1+diag(t(f(1:10))%*%solve(F_N)%*%f(1:10)))

# save predictions in dataframe in order to plot:
austa_test_27$y_pred_N_27 <- y_pred_N_27

# plot the updated model WITH the old model:
ggplot(austa_train_27, aes(x=x_27, y=int_visit)) +
  geom_point() + 
  geom_line(aes(y=y_hat_27), col="red", size=.5) +
  geom_point(data=austa_test_27, aes(x=x_27, y=y_pred_N_27), col="red", size=.5) + 
  geom_ribbon(data=austa_test_27, aes(x=x_27,ymin=y_pred_27_lwr, ymax=y_pred_27_upr), inherit.aes=FALSE, alpha=0.2, fill="red") +
  geom_point(data=austa_test_27, aes(x=x_27,y=int_visit), col="blue", size=.5)+
  geom_line(data=austa_train, aes(x=(x_26-1), y=yhat_N), col="magenta", size=.5, linetype=2) +
  geom_point(data=austa_test, aes(x=(x_26-1), y=y_pred_N), col="magenta", size=.2)





# 5- WLS - example with correlated epsilons

# going back to the OLS model we made first:
ggplot(austa_train, aes(x=year, y=int_visit)) +
  geom_point() + 
  geom_line(aes(y=yhat_ols), col="red", size=.5) 
  
# lets inspect the residuals:
plot(e_ols)
# does it look like white noise?
# or is there a pattern?

qqnorm(e_ols)
qqline(e_ols)

acf(e_ols)
gglagplot(austa)



# define SIGMA matrix
SIGMA <- diag(n)
rho <- 0.7
SIGMA[1:5,1:5]
for (row in 1:n) {
  for (col in 1:n) {
    if(row==col) {
      SIGMA[row,col] <- 1
    } else {
      SIGMA[row,col] <- rho^abs(row-col)
    }
  }
}
print(SIGMA[1:5,1:5]) 


# estimate parameters with WLS
WLS <- solve(t(X)%*%solve(SIGMA)%*%X)%*%(t(X)%*%solve(SIGMA)%*%y)

print(WLS)
# [1,] -356.7172120
# [2,]    0.1804264

# compute standard errors on parameters
yhat_wls <- X%*%WLS
e_wls <- y - yhat_wls
RSS_wls <- t(e_wls)%*%solve(SIGMA)%*%e_wls
sigma2_wls <- as.numeric(RSS_wls/(n - 2))
sqrt(sigma2_wls)
V_wls <- sigma2_wls *  solve(t(X)%*%solve(SIGMA)%*%X)
(sqrt(diag(V_wls))) 
# 24.17891705  0.01213485



# compare in plot with OLS:
ggplot(austa_train, aes(x=year, y=int_visit)) +
  geom_point() + 
  geom_line(aes(y=yhat_wls), col="blue") +
  geom_line(aes(y=yhat_ols), col="red")

# blue is WLS
# red is OLS from before