# I confirm that the attached is my own work, except where clearly indicated in the text.

################################## my_rnorm ####################################

my_rnorm <- function(n,
                     mean = 0,
                     sd = 1){

  # check input is of right form
  if (is.numeric(mean)==FALSE |           # check inputs are numeric
      is.numeric(sd)==FALSE |
      is.numeric(n)==FALSE |
      
      n <= 0 | n!=round(n) |                  # check n is integer
      
      length(n) > 1 |                         # check inputs not vectors
      length(mean) > 1 |
      length(sd) > 1) stop("invalid arguments")
  
  if (n%%2 == 0){
    size <- n/2      # even, can create 2 vectors of independent rnorm values
  } else {
    size <- (n+1)/2   # odd, will generate extra rnorm value (removed later)
  }
  
  a <- runif(size)     # generate enough ~U(0,1) values
  b <- runif(size)
  
  x1 <- rep(0, size)    # pre-allocate
  x2 <- rep(0, size)  
  
  for (i in 1:size){              # use Box-Muller algorithm to generate values
    p <- 2*pi*a[i]
    sq <- sqrt(-2*log(b[i]))
    x1[i] <- sin(p)*sq*sd + mean
    x2[i] <- cos(p)*sq*sd + mean
  }
  
  if (n%%2 == 0){           # if n even, output all generated values
    return(c(x1, x2))
  } else {                  # if n odd, remove 1 value and output the rest
    x2 <- x2[-1]
    return(c(x1, x2))
  }
}

################################# my_rchisq ####################################

my_rchisq <- function(n, df=1) {
  
  if (is.numeric(n)==FALSE |              # check inputs are numeric
      is.numeric(df)==FALSE |
      length(n) > 1 | length(df) > 1 |    # check inputs not vectors
      n <= 0 | n!=round(n) |              # check inputs are integers
      df <= 0 | df!=round(df)){
    stop("invalid arguments") 
  }
 
  chisq <- rep(0, n)               # pre-allocate
  
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
  
  t <- rep(0, n)                     # pre-allocate
  
  for (i in 1:n) t[i] <- z[i]/sqrt(u[i]/df)   # generate values
  
  return(t)
}

################################# Tests ########################################


# Uses **Shapiro-Wilk Normality test**: 
# if p-value > 0.05, implies that the distribution of the data are not
# significantly different from normal distribution.
# In other words, we can assume the normality.
# Input: n (number of samples to return)
# Output: PASSED/FAILED
my_rnorm_test <- function(n){
  
  if (n < 3 | n > 5000) stop("n has to be between 3 and 5000")
  
  x <- my_rnorm(n)
  if (length(x) == n &
      is.numeric(x) &
      shapiro.test(x)$p.value >= 0.05){
    return("PASSED")
  } else {
    return("FAILED")
  }
}

my_rchisq_test <- function(n){
  chisq <- my_rchisq(n)
  if (length(chisq) == n &
      is.numeric(chisq)){
    return("PASSED")
  } else {
    return("FAILED")
  }
}

my_rt_test <- function(n){
  t <- my_rt(n)
  if (length(t) == n &
      is.numeric(t)){
    return("PASSED")
  } else {
    return("FAILED")
  }
}
