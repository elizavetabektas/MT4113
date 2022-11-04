# Starting Values

# Exercise 1

mining_data <- read.csv("./MiningData.csv")

# y <- alpha*(1 - exp( - beta * x))

# As x tends to infinity, y tends to alpha
# take the maximum value of x, initialise alpha to be the angle
alpha_0 <- mining_data$angle[which.max(mining_data$width / mining_data$depth)]
initial_x <- mining_data$width[which.max(mining_data$width / mining_data$depth)] / 
  mining_data$depth[which.max(mining_data$width / mining_data$depth)]
initial_y <- alpha_0

beta_0 <- 34 #randomly chosen, no real reason

theta_0 <- c(alpha_0, beta_0)

model <- function(x, alpha, beta){
  y <- alpha * (1 - exp(-beta*x))
}

y <- mining_data$angle
x <- sort(mining_data$width / mining_data$depth)

y_curve <- sapply(x, model, alpha = alpha_0, beta = beta_0)

plot(x, y)
lines(x,y_curve)




# Objective Function

# Exercise 2

RSS <- function(theta, data){
  alpha <- theta[1]
  beta <- theta[2]
  y <- data$angle
  x <- sort(data$width / data$depth)
  s1 <- 1 - exp(-as.vector(beta)*x)
  s2 <- alpha*s1
  R_theta <- sum((y-s2)^2)
  return(R_theta)
}



# The Newton Update

# Exercise 3
library(numDeriv)


Newton <- function(start, f, data, epsilon, maxit){
  
  theta <- start
  
  convergence <- 2*epsilon
  
  counter <- 0
  
  while (convergence >= epsilon){
    gradient <- grad(f, theta, data = data)
    second_derivative <- hessian(f, theta, data = data)
    delta <- solve(second_derivative, - gradient)
    theta <- theta + delta
    convergence <- norm(delta, type='2') / norm(theta, type='2')
    counter <- counter + 1
    if (counter > maxit){
      break
    }
  }
  
  return(c(theta, counter))
  
}

Newton_output <- Newton(theta_0, RSS, mining_data, 1e-5, 1000)
Newton_alpha <- Newton_output[1]
Newton_beta <- Newton_output[2]
Newton_iterations <- Newton_output[3]



# Using nlm()

# Exercise 4

nlm_result <- nlm(RSS, theta_0, data=mining_data)
nlm_alpha <- nlm_result$estimate[1]
nlm_beta <- nlm_result$estimate[2]


# Using analytical derivatives

# Exercise 5

# Compute gradient vector for objective function f
# INPUTS: theta: parameter vector data: dataframe with
# x and y columns OUTPUTS: vector of gradients in each
# direction
df <- function(theta, data) {
  # compute residuals r for each data point
  data$x <- data$width / data$depth
  pred <- theta[1] * (1 - exp(-theta[2] * data$x))
  r <- data$angle - pred
  # df/da
  dfa <- -2 * (1 - exp(-theta[2] * data$x)) * r
  # df/db
  dfb <- -2 * theta[1] * data$x * exp(-theta[2] * data$x) * r
  return(c(sum(dfa), sum(dfb)))
}

# Compute Hessian matrix for objective function f
# INPUTS: theta: parameter vector data: dataframe with
# x and y columns OUTPUTS: Hessian matrix of second
# derivatives
d2f <- function(theta, data) {
  data$x <- data$width / data$depth
  # compute residuals for each data point
  pred <- theta[1] * (1 - exp(-theta[2] * data$x))
  r <- data$angle - pred
  # compute d2f / da2
  dfaa <- 2 * sum((1 - exp(-theta[2] * data$x))^2)
  # d2f / db2
  dfbb <- 2 * sum(theta[1]^2 * data$x^2 * exp(-2 * theta[2] * data$x) + r * theta[1] * data$x^2 * exp(-theta[2] *
                                                                                          data$x))
  # d2f / dab
  dfab <- 2 * sum(theta[1] * data$x * exp(-theta[2] * data$x) * (1 - exp(-theta[2] * data$x)) - r * data$x *exp(-theta[2] * data$x))
  # H = hessian
  H <- matrix(c(dfaa, dfab, dfab, dfbb), nrow = 2, ncol = 2)
  return(H)
}





Newton2 <- function(start, f, data, epsilon, maxit, gfn, Hfn){
  
  theta <- start
  
  convergence <- 2*epsilon
  
  counter <- 0
  
  while (convergence >= epsilon){
    gradient <- gfn(theta, data)
    second_derivative <- Hfn(theta, data)
    print(theta)
    delta <- solve(second_derivative, - gradient)
    theta <- theta + delta
    convergence <- norm(delta, type='2') / norm(theta, type='2')
    counter <- counter + 1
    print(counter)
    if (counter > maxit){
      break
    }
  }
  
  return(c(theta, counter))
}


Newton2_output <- Newton2(theta_0, RSS, mining_data, 1e-5, 1000, df, d2f)
Newton2_alpha <- Newton2_output[1]
Newton2_beta <- Newton2_output[2]
Newton2_iterations <- Newton2_output[3]

# (b) not completed, function needs options for approximate vs analytical derivatives
# (c) not completed as function doesn't work "system is computationally singular"
#     i.e. determinant of matrix is 0