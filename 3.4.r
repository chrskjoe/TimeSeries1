file_path <- 'As1Test.xlsx'
data <- read_excel(file_path)

yt <- as.numeric(data[1, -1])
xt <- seq(from = 2018, by = 1/12, length.out = length(yt))
n = length(yt)


f <- function(j) rbind(1, j)
L <- matrix(c(1.,0., 1.,1.),
            byrow=TRUE, nrow=2)
Linv <- solve(L) 

lambda <- 0.9

i <- 1
(F_N <-  (lambda^0) * f(0)%*%t(f(0)))
(h_N <-  (lambda^0) * f(0) * yt[i])
print(F_N)
print(h_N)
#theta_N <- solve(F_N)%*%h_N
# does not work - F_N is not invertible!

i <- 2
F_N <- F_N + lambda^(i-1) * f(-(i-1)) %*% t(f(-(i-1)))  
h_N <- lambda * Linv %*% h_N + f(0)*yt[i]

theta_N <- solve(F_N)%*%h_N
print(F_N)
print(h_N)
# now it works (we used two observations and we are estimating two parameters...)


for (i in 3:26){
  F_N <- F_N + lambda^(i-1) * f(-(i-1)) %*% t(f(-(i-1)))  
  h_N <- lambda * Linv %*% h_N + f(0)*yt[i]
  print(i)
  print(F_N)
  print(h_N)
}