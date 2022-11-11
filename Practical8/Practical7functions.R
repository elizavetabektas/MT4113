
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
