### Example for TSA Lecture 2
# 1- Fit OLS to most recent data (partial data-set)
# 2- Fit WLS with "lambda-weights"
# 3- Local Trend Model - iterative updates
# 4- Local Trend Model - onestep predictions

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



# 1- Fit OLS to most recent data (partial data-set)

nparams <- 2

X <- cbind(1, austa_data$year)
y <- cbind(austa_data$int_visit)


fit_austa <- function(start, stop) {
  
  n <- stop - start + 1
  
  # start=21
  # stop=30
  OLS <- solve(t(X[start:stop,])%*%X[start:stop,])%*%t(X[start:stop,])%*%y[start:stop]
  
  austa_data$yhat <- X%*%OLS
  Xpred <- cbind(1, seq(1980+stop, 1980+stop+9, 1))
  ypred <- Xpred%*%OLS
  pred <- data.frame(Xpred, ypred)
  
  RSS_ols <- sqrt(sum((austa_data$yhat[start:stop] - y[start:stop])^2))
  sigma2_ols <- as.numeric(RSS_ols/(n - nparams))
  Vmatrix_pred <- sigma2_ols*(1+(Xpred%*%solve(t(X[start:stop,])%*%X[start:stop,]))%*%t(Xpred))
  pred$ypred_lwr <- pred$ypred - 1.96*sqrt(diag(Vmatrix_pred))
  pred$ypred_upr <- pred$ypred + 1.96*sqrt(diag(Vmatrix_pred))
  
  myplot <- ggplot(austa_data, aes(x=year, y=int_visit)) +
    geom_point() + 
    geom_point(data=austa_data[start:stop,], col="red") + 
    geom_line(data=austa_data[1:(stop),], aes(y=yhat), col="red", size=.5) + 
    geom_point(data=pred, aes(x=Xpred[,2], y=ypred), col="red", size=.5) + 
    geom_ribbon(data=pred, aes(x=Xpred[,2],ymin=ypred_lwr, ymax=ypred_upr), inherit.aes=FALSE, alpha=0.2, fill="red") +
    ylim(0,9.5) + xlim(1980, 2026)
  
  print(myplot)
}

fit_austa(1,26)
fit_austa(17,26)
fit_austa(21,26)
fit_austa(25,26)




# 2- Fit WLS with "lambda-weights"

n <- 26
lambda = 0.6
weights <- lambda^((n-1):0)
# plot the weights:
barplot(weights, names=1:26)


SIGMA <- diag(n)
for (i in 1:n) {
  SIGMA[i,i] <- 1/lambda^(n-i)
}
print(SIGMA[20:26,20:26])

# estimate parameters with WLS (using only first 26 observations)
WLS <- solve(t(X[1:26,])%*%solve(SIGMA)%*%X[1:26,])%*%(t(X[1:26,])%*%solve(SIGMA)%*%y[1:26])
yhat_wls <- X[1:26,]%*%WLS

# estimate parameters with unweighted OLS (for comparison)
OLS <- solve(t(X[1:26,])%*%X[1:26,])%*%(t(X[1:26,])%*%y[1:26])
yhat_ols <- X[1:26,]%*%OLS


# plot
ggplot(austa_data[1:26,], aes(x=year, y=int_visit)) +
  geom_point() + 
  geom_line(aes(y=yhat_wls), col="blue") +
  geom_line(aes(y=yhat_ols), col="red", linetype=2)




# 3- Local Trend Model - iterative updates


f <- function(j) rbind(1, j)
L <- matrix(c(1.,0., 1.,1.),
            byrow=TRUE, nrow=2)
Linv <- solve(L) 


i <- 1
(F_N <-  (lambda^0) * f(0)%*%t(f(0)))
(h_N <-  (lambda^0) * f(0) * y[i])

theta_N <- solve(F_N)%*%h_N
# does not work - F_N is not invertible!

i <- 2
F_N <- F_N + lambda^(i-1) * f(-(i-1)) %*% t(f(-(i-1)))  
h_N <- lambda * Linv %*% h_N + f(0)*y[i]

theta_N <- solve(F_N)%*%h_N
# now it works (we used two observations and we are estimating two parameters...)

# plot
yhat_N <- t(f(-(i-1):(26-i)))%*%theta_N
ggplot(austa_data[1:26,], aes(x=year, y=int_visit)) +
  geom_point() + 
  geom_point(data=austa_data[1:i,], col="blue") + 
  geom_line(data=austa_data[1:i,], aes(y=yhat_N[1:i]), col="blue")+
  geom_line(aes(y=yhat_ols), col="red", linetype=2) + 
  xlim(1980, 2005) + ylim(0,5.5) + 
  ggtitle(paste0("N = ", i))

# update iteratively:
for (i in 3:26){
  F_N <- F_N + lambda^(i-1) * f(-(i-1)) %*% t(f(-(i-1)))  
  h_N <- lambda * Linv %*% h_N + f(0)*y[i]
  theta_N <- solve(F_N)%*%h_N
  
  yhat_N <- t(f(-(i-1):(26-i)))%*%theta_N
  plot_N <- ggplot(austa_data[1:26,], aes(x=year, y=int_visit)) +
    geom_point() + 
    geom_point(data=austa_data[1:i,], col="blue") + 
    geom_line(data=austa_data[1:i,], aes(y=yhat_N[1:i]), col="blue")+
    geom_line(aes(y=yhat_ols), col="red", linetype=2) + 
    xlim(1980, 2005) + ylim(0,5.5) + 
    ggtitle(paste0("N = ", i))
  
  print(plot_N)
}



# 4- Local Trend Model - onestep predictions

i <- 1
(F_N <-  (lambda^0) * f(0)%*%t(f(0)))
(h_N <-  (lambda^0) * f(0) * y[i])

# want to save onestep predictions in dataframe (for lambda = 0.6):
austa_data$onestep_lamb_06  <- NA

# update iteratively:
for (i in 2:25){
  F_N <- F_N + lambda^(i-1) * f(-(i-1)) %*% t(f(-(i-1)))  
  h_N <- lambda * Linv %*% h_N + f(0)*y[i]
  theta_N <- solve(F_N)%*%h_N
  
  yhat_N <- t(f(-(i-1):(26-i)))%*%theta_N
  austa_data$onestep_lamb_06[i+1] <- yhat_N[i+1]
  plot_N <- ggplot(austa_data[1:26,], aes(x=year, y=int_visit)) +
    geom_point() + 
    geom_point(data=austa_data[1:i,], col="blue") + 
    geom_line(data=austa_data[1:i,], aes(y=yhat_N[1:i]), col="blue")+
    geom_line(data=austa_data[1:(i+1),], aes(y=yhat_N[1:(i+1)]), col="blue", linetype=2)+
    geom_point(data=austa_data[i+1,], aes(y=yhat_N[i+1]), col="blue", size=4)+
    geom_line(aes(y=yhat_ols), col="red", linetype=2) + 
    xlim(1980, 2005) + ylim(0,5.5) + 
    ggtitle(paste0("N = ", i))
  
  print(plot_N)
}


ggplot(austa_data[1:26,], aes(x=year, y=int_visit)) +
  geom_point(data=austa_data, aes(y=onestep_lamb_06), col="blue", size=4)+
  geom_line(aes(y=yhat_ols), col="red", linetype=2) + 
  xlim(1980, 2005) + ylim(0,5.5) + 
  ggtitle(paste0("N = ", i)) +
  geom_point() 
  

# try another lambda
lambda <- 0.9
i <- 1
(F_N <-  (lambda^0) * f(0)%*%t(f(0)))
(h_N <-  (lambda^0) * f(0) * y[i])

# want to save onestep predictions in dataframe (for lambda = 0.6):
austa_data$onestep_lamb_09  <- NA

# update iteratively:
for (i in 2:25){
  F_N <- F_N + lambda^(i-1) * f(-(i-1)) %*% t(f(-(i-1)))  
  h_N <- lambda * Linv %*% h_N + f(0)*y[i]
  theta_N <- solve(F_N)%*%h_N
  
  yhat_N <- t(f(-(i-1):(26-i)))%*%theta_N
  austa_data$onestep_lamb_09[i+1] <- yhat_N[i+1]
  plot_N <- ggplot(austa_data[1:26,], aes(x=year, y=int_visit)) +
    geom_point() + 
    geom_point(data=austa_data[1:i,], col="blue") + 
    geom_line(data=austa_data[1:i,], aes(y=yhat_N[1:i]), col="blue")+
    geom_line(data=austa_data[1:(i+1),], aes(y=yhat_N[1:(i+1)]), col="blue", linetype=2)+
    geom_point(data=austa_data[i+1,], aes(y=yhat_N[i+1]), col="blue", size=4)+
    geom_line(aes(y=yhat_ols), col="red", linetype=2) + 
    xlim(1980, 2005) + ylim(0,5.5) + 
    ggtitle(paste0("N = ", i))
  
  print(plot_N)
}


ggplot(austa_data[1:26,], aes(x=year, y=int_visit)) +
  geom_point(data=austa_data, aes(y=onestep_lamb_06), col="blue", size=4)+
  geom_point(data=austa_data, aes(y=onestep_lamb_09), col="green", size=4)+
  geom_line(aes(y=yhat_ols), col="red", linetype=2) + 
  xlim(1980, 2005) + ylim(0,5.5) + 
  ggtitle(paste0("N = ", i)) +
  geom_point() 


lambda <- 0.3
i <- 1
(F_N <-  (lambda^0) * f(0)%*%t(f(0)))
(h_N <-  (lambda^0) * f(0) * y[i])

# want to save onestep predictions in dataframe (for lambda = 0.6):
austa_data$onestep_lamb_03  <- NA

# update iteratively:
for (i in 2:25){
  F_N <- F_N + lambda^(i-1) * f(-(i-1)) %*% t(f(-(i-1)))  
  h_N <- lambda * Linv %*% h_N + f(0)*y[i]
  theta_N <- solve(F_N)%*%h_N
  
  yhat_N <- t(f(-(i-1):(26-i)))%*%theta_N
  austa_data$onestep_lamb_03[i+1] <- yhat_N[i+1]
  plot_N <- ggplot(austa_data[1:26,], aes(x=year, y=int_visit)) +
    geom_point() + 
    geom_point(data=austa_data[1:i,], col="blue") + 
    geom_line(data=austa_data[1:i,], aes(y=yhat_N[1:i]), col="blue")+
    geom_line(data=austa_data[1:(i+1),], aes(y=yhat_N[1:(i+1)]), col="blue", linetype=2)+
    geom_point(data=austa_data[i+1,], aes(y=yhat_N[i+1]), col="blue", size=4)+
    geom_line(aes(y=yhat_ols), col="red", linetype=2) + 
    xlim(1980, 2005) + ylim(0,5.5) + 
    ggtitle(paste0("N = ", i))
  
  print(plot_N)
}


ggplot(austa_data[1:26,], aes(x=year, y=int_visit)) +
  geom_point(data=austa_data, aes(y=onestep_lamb_03), col="magenta", size=4)+
  geom_point(data=austa_data, aes(y=onestep_lamb_06), col="blue", size=4)+
  geom_point(data=austa_data, aes(y=onestep_lamb_09), col="green", size=4)+
  geom_line(aes(y=yhat_ols), col="red", linetype=2) + 
  xlim(1980, 2005) + ylim(0,5.5) + 
  ggtitle(paste0("N = ", i)) +
  geom_point() 

