# STARTING VALUES

# Exercise 1

mining_data <- read.csv("./MiningData.csv")

beta_function <- function(x,y){
  beta <- - log(1 / y/34) / x
  return(beta)
}

beta_0 <- beta_function(5,5)

model <- function(x, beta){
  y <- 34 * (1 - exp(-beta*x))
}

y <- mining_data$angle
x <- sort(mining_data$width / mining_data$depth)

y_curve <- sapply(x, model, beta = beta_0)

plot(x, y)
lines(x,y_curve)


# OBJECTIVE FUNCTION

# Exercise 2

RSS <- function(beta, data){
  y <- data$angle
  x <- sort(data$width / data$depth)
  s1 <- 1 - exp(-as.vector(beta)*x)
  s2 <- 34*s1
  R_theta <- sum((y-s2)^2)
  return(R_theta)
}


# GRADIENT AND SECOND DERIVATIVE

# Exercise 3
library(numDeriv)

gradient <- grad(RSS, beta_0, data = mining_data)

second_derivative <- hessian(RSS, beta_0, data = mining_data)



# THE NEWTON UPDATE

# Exercise 4

Newton <- function(start, f, data, epsilon, maxit){
  
  beta <- start
  
  convergence <- 2*epsilon
  
  counter <- 0
  
  while (convergence >= epsilon){
    gradient <- grad(f, beta, data = data)
    second_derivative <- hessian(f, beta, data = data)
    delta <- - gradient / second_derivative
    beta <- beta + delta
    convergence <- abs(delta)/abs(beta)
    counter <- counter + 1
    if (counter > maxit){
      break
    }
  }
  
  return(c(beta, counter))
  
}

Newton_output <- Newton(beta_0, RSS, mining_data, 1e-5, 1000)
Newton_beta <- Newton_output[1]
Newton_iterations <- Newton_output[2]


# USING OPTIMIZE()

# Exercise 5

optimize_output <- optimize(RSS, c(-100,100), data=mining_data)
optimize_value <- optimize_output$minimum
optimize_iterations <- optimize_output$objective


