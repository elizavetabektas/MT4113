# Starting Values

# Exercise 1

mining_data <- read.csv("./MiningData.csv")

mining_data$x <- mining_data$width / mining_data$depth
mining_data$y <- mining_data$angle

# y <- alpha*(1 - exp( - beta * x))

# As x tends to infinity, y tends to alpha
# take the maximum value of x, initialise alpha to be the angle
alpha_0 <- mining_data$angle[which.max(mining_data$width / mining_data$depth)]
initial_x <- mining_data$width[which.max(mining_data$width / mining_data$depth)] / 
  mining_data$depth[which.max(mining_data$width / mining_data$depth)]
initial_y <- alpha_0

beta_0 <- 2 #randomly chosen, no real reason

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

library(numDeriv)

# Compute gradient vector for objective function f
# INPUTS: theta: parameter vector data: dataframe with
# x and y columns OUTPUTS: vector of gradients in each
# direction
df <- function(theta, data) {
  # compute residuals r for each data point
  pred <- theta[1] * (1 - exp(-theta[2] * data$x))
  r <- data$y - pred
  # df/da
  dfa <- -2 * (1 - exp(-theta[2] * data$x)) * r
  # df/db
  dfb <- -2 * theta[1] * data$x * exp(-theta[2] * data$x) *
    r
  return(c(sum(dfa), sum(dfb)))
}

# Compute Hessian matrix for objective function f
# INPUTS: theta: parameter vector data: dataframe with
# x and y columns OUTPUTS: Hessian matrix of second
# derivatives
d2f <- function(theta, data) {
  # compute residuals for each data point
  pred <- theta[1] * (1 - exp(-theta[2] * data$x))
  r <- data$y - pred
  # compute d2f / da2
  dfaa <- 2 * sum((1 - exp(-theta[2] * data$x))^2)
  # d2f / db2
  dfbb <- 2 * sum(theta[1]^2 * data$x^2 * exp(-2 * theta[2] *
                                                data$x) + r * theta[1] * data$x^2 * exp(-theta[2] *
                                                                                          data$x))
  # d2f / dab
  dfab <- 2 * sum(theta[1] * data$x * exp(-theta[2] *
                                            data$x) * (1 - exp(-theta[2] * data$x)) - r * data$x *
                    exp(-theta[2] * data$x))
  # H = hessian
  H <- matrix(c(dfaa, dfab, dfab, dfbb), nrow = 2, ncol = 2)
  return(H)
}

# (a)

# Approximate derivatives:
grad(RSS, theta_0, data = mining_data)
hessian(RSS, theta_0, data = mining_data)

# Exact derivatives:
df(theta_0, data = mining_data)
d2f(theta_0, data = mining_data)

# (b)

# Optimise a function f using Newton's method INPUTS:
# start: vector of starting values f: function to be
# minimised data: dataframe with x and y columns gfn:
# (optional) gradient function of f, uses finite
# differencing otherwise Hfn: (optional) hessian
# function of f, uses finite differencing otherwise
# maxit: maximum number of iterations to try tol:
# tolerance is e where iterations stop when current
# iterate x changes by less than 100e% OUTPUTS: a list
# with elements: - estimate: a vector of the optimal
# estimates - value: value of the objective function
# at the optimum - g: gradient vector at the optimum -
# H: hessian at the optimum - conv: is TRUE if
# convergence was satisfied, otherwise may not have
# converged on optimum - niter: number of iterations
# taken
Newton <- function(start, f, data, gfn = NULL, Hfn = NULL,
                   maxit = 1000, tol = 1e-10) {
  # if not gradient or Hessian then use numDeriv
  if (is.null(gfn)) {
    gfn <- function(theta, data) {
      grad(f, theta, data = data)
    }
  }
  if (is.null(Hfn)) {
    Hfn <- function(theta, data) {
      hessian(f, theta, data = data)
    }
  }
  # set starting values
  theta <- start
  # setup loop
  iter <- 0
  loop <- TRUE
  conv <- FALSE
  while (loop) {
    iter <- iter + 1
    # compute gradient and hessian
    g <- gfn(theta, data)
    H <- Hfn(theta, data)
    # solve for step to take
    delta <- solve(H, -g)
    # check stopping criterion
    if (norm(delta, type='2')/norm(theta, type='2') < tol | iter > maxit) {
      loop <- FALSE
    }
    # update theta
    theta <- theta + delta
  }
  # check convergence
  if (norm(delta, type='2')/norm(theta, type='2') < tol) {
    conv <- TRUE
  } else {
    conv <- FALSE
  }
  return(list(estimate = theta, value = f(theta, data),
              g = gfn(theta, data), H = Hfn(theta, data), conv = conv,
              niter = iter))
}

# (c)

# Run with no analytic derivatives
system.time(opt1 <- Newton(theta_0, RSS, mining_data, maxit = 10))
opt1$niter
opt1$estimate
opt1$value
# Run with known gradient, but no Hessian
system.time(opt2 <- Newton(theta_0, RSS, mining_data, gfn = df))
opt2$niter
opt2$estimate
opt2$value
# Run with known analytic derivatives
system.time(opt3 <- Newton(theta_0, RSS, mining_data, gfn = df,
                           Hfn = d2f))
opt3$niter
opt3$estimate
opt3$value
# Compare with nlm()
nlm_alpha
nlm_beta
