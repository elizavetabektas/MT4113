source("./Practical7functions.R")


covid <- read.csv("./CovidData.csv")


# THE GAUSS-NEWTON METHOD

alpha_0 <- 11
beta_0 <- 0.12
theta_0 <- c(alpha_0, beta_0)

model <- function(x, alpha, beta){
  y <- alpha * exp(beta*x)
}

y <- covid$y
x <- covid$x

y_curve <- sapply(x, model, alpha = alpha_0, beta = beta_0)

plot(x, y)
lines(x,y_curve)


# Exercise 1

library(numDeriv)

resids <- function(theta, data){
  resid <- data$y - theta[1]*exp(theta[2]*data$x)
  return(resid)
}

gauss_newton <- function(start, # vector or starting values
                         f, # prediction function
                         data, # dataframe used by f
                         maxit = 1000,
                         epsilon = 1e-5){ 
  theta <- start
  
  convergence <- 2*epsilon
  
  counter <- 0
  while (convergence >= epsilon){
    jcbn <- jacobian(f, theta, data = data)
    residss <- f(theta, data)
    g <- t(jcbn) %*% residss
    H <- t(jcbn) %*% jcbn
    delta <- solve(H, -g)
    theta <- theta + delta
    convergence <- norm(delta, type='2') / norm(theta, type='2')
    counter <- counter + 1
    if (counter > maxit){
      break
    }
  }
  
  return(c(theta, counter))
} 

out1 <- gauss_newton(theta_0, resids, covid)
out1[1] # alpha = 9.123403
out1[2] # beta = 0.1230119
out1[3] # number of iterations


# USING THE NLS() FUNCTION

# Exercise 2

out2 <- nls(y ~ a * exp(b * x),
            data = covid,
            start = list(a = theta_0[1], b = theta_0[2]))

# alpha = 9.123
# beta = 0.123 



# COMPARING WITH OTHER METHODS

# Exercise 3

# TO BE COMPLETED

#Newton_output <- Newton(theta_0, resids, covid, tol = 1e-5, maxit = 1000)
#Newton_alpha <- Newton_output[1]
#Newton_beta <- Newton_output[2]
#Newton_iterations <- Newton_output[3]
