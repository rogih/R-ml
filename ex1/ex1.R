#### Exercise 1 - Andrew Ng - Machine Learning course on Coursera

#### warmup
A <- diag(5)

####


#### plotting
data <- read.table("ex1data1.txt", sep = ",", header = F)
head(data)
x <- data[, 1]
y <- data[, 2]
# size of the dataset
m <- length(y)

# plot function
plot(x, y, col = "red", pch = 4, cex = 1.1, lwd = 2, xlab = "Profit in $10,000s", ylab = "Population of City in 10,000s")

####

#### gradient descent
# cost function
# this is already the vectorized implementation
computeCost <- function(x, y, theta) {
  m <- length(y)
  J <- 0
  hip <- (x %*% (theta)) - y
  J <- (t(hip) %*% hip) / (2 * m)
  return(J)
}

# adding ones
x <- cbind(rep(1, m), x)
# first value for theta
theta <- rep(0, 2)
# cost computed, should be 32.07
computeCost(x, y, theta)

# cost computed, should be 54.24
computeCost(x, y, c(-1, 2))

# gradient descent function
# this is already the vectorized implementation
gradientDescent <- function(x, y, theta, alpha, n.iters) {
  m <- length(y)
  j.history <- rep(0, n.iters + 1)

  for (i in 1:n.iters) {
    theta <- theta - t((alpha / m * t((x %*% theta - y)) %*% x))
    j.history[i] <- computeCost(x, y, theta)
  }
  j.history[i] <- computeCost(x, y, theta)
  list(theta = theta, j.history = j.history)
}

# gradient descent settings
iterations <- 1500
alpha <- 0.02

# run gradient descent
grad.desc <- gradientDescent(x, y, theta, alpha, iterations)
# theta estimated, should be -3.6303 and  1.1664
grad.desc$theta

# plot with fitted line
plot(x[, 2], y, col = "red", pch = 4, cex = 1.1, lwd = 2, xlab = "Profit in $10,000s", ylab = "Population of City in 10,000s")
lines(x[, 2], x %*% theta, col = "blue")
legend("bottomright", c("Training data", "Linear regression"), pch = c(4, NA), col = c("red", "blue"), lty = c(NA, 1))

# predict values for population sizes of 35,000 and 70,000
predict1 <- c(1, 3.5) %*% theta
predict1*10000
predict2 <- c(1, 7) %*% theta
predict2*10000



####  visualizing J(theta.0, theta.1) 

theta0.vals <- seq(-10, 10, length.out=100)
theta1.vals <- seq(-2, 4, length.out=100)

j.vals <- matrix(0,length(theta0.vals), length(theta1.vals))

# Calculating j.vals over the grid points
for (i in 1:length(theta0.vals)) {
  for (j in 1:length(theta1.vals)) {
    j.vals[i,j] <- computeCost(x, y, c(theta0.vals[i], theta1.vals[j]))
  }
}
# plotting the surface
persp(theta0.vals,theta1.vals,j.vals)

# contour plot
contour(theta0.vals, theta1.vals, j.vals, xlab=expression(theta.0), ylab=expression(theta.1), drawlabels = FALSE)
theta <- grad.desc$theta
points(theta[1], theta[2], pch=4, cex=2,col="red",lwd=2)



#### multivariate vectorized
data <- read.table('ex1data2.txt',sep = ',')
x <- data[,1:2]
y <- data[,3]
m <- length(y)
m        
head(x)

# feature normalization
x <- scale(x)
x <- cbind(rep(1,m),x)

alpha = 0.01
n.iters = 400
theta <- (rep(0,3))

grad.desc <- gradientDescent(x, y, theta, alpha , n.iters)
theta <- grad.desc$theta
j.history <- grad.desc$j.history


# plot the convergence graph
plot(1:length(j.history), j.history, type="l", col="blue", xlab="Number of Iterations", ylab="Cost J")

# Estimate the price of a 1650 sq-ft, 3 br house
predictt = c(1,1650,3)
price <- predictt*(theta);
sum(price)

#### Normal equations 

data <- read.table('ex1data2.txt',sep =',')
x <- data[, 1:2]
y <- data[, 3]
m <- length(y)

x <- cbind(rep(1,m),x)
x <- as.matrix(x)


# Normal equation
theta <- solve(t(x) %*% x) %*% t(x) %*% y
theta
