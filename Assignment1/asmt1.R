# I confirm that the attached is my own work, except where clearly indicated in the text.

################################## my_rnorm ####################################

my_rnorm <- function(n, mean = 0, sd = 1) {

  if (is.numeric(mean)==FALSE |           # check inputs are numeric
      is.numeric(sd)==FALSE |
      is.numeric(n)==FALSE |
      n <= 0 | n!=round(n) |                  # check n is integer
      length(n) > 1 |                         # check inputs are not vectors
      length(mean) > 1 |
      length(sd) > 1) stop("invalid arguments")
  
  # handle even & odd cases differently
  ifelse(n%%2 == 0, size <- n/2, size <- (n+1)/2) 
  
  a <- runif(size)                  # generate ~U(0,1) values
  b <- runif(size)

  # use Box-Muller algorithm to generate values
  x1 <- sin(2*pi*a)*sqrt(-2*log(b))*sd + mean 
  x2 <- cos(2*pi*a)*sqrt(-2*log(b))*sd + mean
  
  ifelse(n%%2 == 0,
         return(c(x1, x2)),       # if n even, output all generated values
         return(c(x1, x2[-1])))   # if n odd, remove 1 value and output the rest
}

################################# my_rchisq ####################################

my_rchisq <- function(n, df=1) {
  
  if (is.numeric(n)==FALSE |              # check inputs are numeric
      is.numeric(df)==FALSE |
      length(n) > 1 | length(df) > 1 |    # check inputs not vectors
      n <= 0 | n!=round(n) |              # check inputs are integers
      df <= 0 | df!=round(df))    stop("invalid arguments")
 
  chisq <- rep(0, n)                             # preallocation
  for (i in 1:n) chisq[i] <- sum(my_rnorm(df)**2) # implement algorithm
  return(chisq)
}

################################## my_rt #######################################

my_rt <- function(n, df=1){
  
  if (is.numeric(n)==FALSE |              # check inputs are numeric
      is.numeric(df)==FALSE |
      length(n) > 1 | length(df) > 1 |    # check inputs not vectors
      n <= 0 | n!=round(n) |              # check inputs are integers
      df <= 0 | df!=round(df)){
    stop("invalid arguments") 
  }
  
  z <- my_rnorm(n)                   # generate enough ~N(0,1) values
  u <- my_rchisq(n, df)              # generate enough ~ChiSq(n) values
  
  t <- rep(0, n)                              # preallocation
  for (i in 1:n) t[i] <- z[i]/sqrt(u[i]/df)   # generate values

  return(t)
}

################################# Tests ########################################

# There are two tests: numerical and visual. They work on all above functions.

# test_numerical checks if output is of correct length, is numeric and passes
# a p-value test (>=0.05 implies the distribution of the data are not
# significantly different from the specified distribution, so we can assume
# it comes from that distribution)
# All p-value tests have a Kolmogorov-Smirnov test, which tests whether or not a
# sample comes from a certain distribution.
# my_rnorm also has a Shapiro-Wilk Normality test

# INPUTS : my_function (one of above functions to be tested)
#          n (number of samples, corresponds to an input to all above functions)
#               for my_rnorm, has to be >3, <5000 for shapiro.test
#          mean, sd (correspond to my_rnorm input)
#          df (corresponds to my_rchisq and my_rt inputs)
# OUTPUTS: PASSED/FAILED
test_numerical <- function(my_function, n, mean=0, sd=1, df=1){
  
  # if function we're testing is my_rnorm, use shapiro.test and ks.test for pvalue
  if (deparse(substitute(my_function)) == "my_rnorm"){
    
    # for shapiro.test, n has to be between 3 and 5000
    if (n < 3 | n > 5000) stop("n has to be between 3 and 5000")
    
    x <- my_function(n, mean, sd)
    pvalue_test <- (shapiro.test(x)$p.value >= 0.05 &
                      ks.test(x, "pnorm", mean, sd)$p.value >= 0.05)
    
  # if function is my_rchisq or my_rt, generate values from those distributions
  } else {
    x <- my_function(n, df)
    
    # calculate the p-values for each separately (different ks.test inputs)
    if (deparse(substitute(my_function)) == "my_rchisq"){
      pvalue_test <- ks.test(x, "pchisq", df)$p.value >= 0.05
    } else {
      pvalue_test <- ks.test(x, "pt", df)$p.value >= 0.05
    }
  }
  
  if (length(x) == n &      # check output of my_function is of correct length
      is.numeric(x) &       # check output is numeric
      pvalue_test){         # check pvalue test passes
    return("PASSED")
  } else {
    return("FAILED")
  }
}

# test_graphical plots histogram of values from my_function and a line graph from
# the actual distribution so we can visually compare them
# INPUTS : my_function (one of above functions to be tested)
#          n (number of samples, corresponds to an input to all above functions)
#          mean, sd (correspond to my_rnorm input)
#          df (corresponds to my_rchisq and my_rt inputs)
# OUTPUTS: plot (histogram and line)
test_visual <- function(my_function, n, mean=0, sd=1, df=1){
  
  # if my_function is my_rnorm, generate my_rnorm values and dnorm values
  if (deparse(substitute(my_function)) == "my_rnorm"){
    x <- my_function(n, mean, sd)
    x_vals <- seq(min(x), max(x), length=400)
    y_vals <- dnorm(x_vals, mean, sd)
    
  # if my_function is my_tchisq/my_rt, generate values from those distributions
  } else {
    x <- my_function(n, df)
    x_vals <- seq(min(x), max(x), length=400)
    
    # generate values from dchisq or dt respectively
      if (deparse(substitute(my_function)) == "my_rchisq"){
        y_vals <- dchisq(x_vals, df)
      } else {
        y_vals <- dt(x_vals, df)
      }
  }
  # plot graph
  hist(x, prob=TRUE, border="cyan")
  lines(x_vals, y_vals, col="magenta")
}

######################### COMMENTS ABOUT TESTS #################################
# test_numerical will sometimes fail because my_* functions are random number 
# generators, e.g the generated data ends up all on one side of the
# distribution, the test will fail
# by plotting test_visual, we can see that my_* functions can fail the visual
# test on occasion too
# this happens to rnorm, rchisq and rt functions too