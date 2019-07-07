#### Exercise 2 - Andrew Ng - Machine Learning course on Coursera

#### Sigmoid function
sigmoid <- function (z){
  sigmoid <- 1 /(1 + exp(-z))
}


#### Part 1 and 2

# loading data for the first part
data <- read.table('ex2data1.txt',sep = ',')
x <- data[,1:2]
y <- data[,3]

# creating labels for the plot
yfac <- factor(y)
# ploting the data
plot(x[,1],x[,2],xlab="Exam 1", ylab="Exam 2",pch=(1:2)[yfac],col= c("#00AFBB", "#E7B800")[yfac])

m <- dim(x)[1]
n <- dim(x)[2]

x <- cbind(rep(1,n),x)
x <- as.matrix(x)

# cost and gradient functions
costFunction  <- function(theta,x, y) {
  m <- length(y)
  h <- sigmoid(x %*% theta)
  J <- (t(-y) %*% log(h) - t(1 - y) %*% log(1 - h)) / m
  grad = (t(x)%*%(sigmoid(x%*%theta)-y))/m;
  return(list(grad = grad, J = J))
}

theta <- rep(0,n+1)
cost.log = costFunction(theta, x, y)
# Cost should be 0.693  and  gradients -0.1000 -12.0092 -11.2628
cost.log

theta <- c(-24,0.2,0.2)
cost.log = costFunction(theta, x, y)
# Cost should be 0.218  and  gradients 0.043 2.566 2.647
cost.log

#### Part 3: Optimizing using optim  
costFunctionn  <- function(theta,x, y) {
  m <- length(y)
  h <- sigmoid(x %*% theta)
  J <- (t(-y) %*% log(h) - t(1 - y) %*% log(1 - h)) / m
  grad = (t(x)%*%(sigmoid(x%*%theta)-y))/m;
  return(J)
}

optimRes <- optim(par = theta,fn=costFunctionn, method="BFGS",x=x,y=y, control = list(maxit = 400))
theta.est <- optimRes$par


#### Part 4: prediction and accurary
# for a student with scores 45 and 85, we predict an admission , should be 0.775 +/- 0.002
prob <- sigmoid(c(1,45,85)%*%t(t(theta.est)))

predict <- function(theta, x) {
  m <- dim(x)[1]
  p <- rep(0,m)
  p[sigmoid(x %*% theta) >= 0.5] <- 1
  return(p)
}

p <- predict(theta.est, x)
mean(p == y) * 100

