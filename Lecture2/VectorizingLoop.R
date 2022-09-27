## Loading in the SOI data as in lecture 2.

library(DAAG)
data(bomsoi)
soi <- bomsoi$SOI
est.mu <- mean(soi)
est.sd <- sd(soi)


## Then I've turned attempt 3 from section 2.4 of the lectures
## into a function:

testfunction1<-function(n=100){
sample <- NULL 
while (length(sample) < n) {
  x <- runif(1, -20, 20)
  y <- runif(1, 0, 0.06)
  f <- dnorm(x, est.mu, est.sd)
  if (y <= f) sample <- c(sample, x)
}
return(sample)
}


# "In our SOI example, it is difficult to use vectorisation 
# because the number of iterations is unknown and so a loop 
# is necessary"

## We can improve things though

system.time(tmp1<-testfunction1(100))

# things are very quick for n = 100, so we'll try something a little larger

system.time(tmp1<-testfunction1(100000)) # 34.146 seconds


############################
############################
############################
############################


# Approach 1
## Preallocation

testfunction2<-function(n=100){
  sample <- rep(NA,n) 
  i<-1
  while (i < n) {
    x <- runif(1, -20, 20)
    y <- runif(1, 0, 0.06)
    f <- dnorm(x, est.mu, est.sd)
    if (y <= f) {
      sample[i] <- x
      i<-i+1
    }
  }
  return(sample)
}


system.time(tmp2<-testfunction2(100000)) # 1.376 seconds


############################
############################
############################
############################


# Approach 2
## Partial vectorization

testfunction3<-function(n=100){
sample <- NULL 
while (length(sample) < n) {
  x <- runif(n, -20, 20)
  y <- runif(n, 0, 0.06)
  f <- dnorm(x, est.mu, est.sd)
  sample <- c(sample, x[y <= f])
}
return(sample[1:n])
}


system.time(tmp3<-testfunction3(100000)) # 0.081 seconds



############################
############################
############################
############################


# Approach 3
## Parallelization

# First, I'm simplifying to a function (similar to 
# the first function) that only returns one value

testfunction4help<-function(){
  sample<-NA
  while (TRUE) {
    x <- runif(1, -20, 20)
    y <- runif(1, 0, 0.06)
    f <- dnorm(x, est.mu, est.sd)
    if (y <= f){
      return(x)
    } 
  }
  
}

# Now I use lapply to 'apply' the function to a list of numbers 
# The numbers aren't used, this is just a way of running the 
# function multiple times

## Note that this essentially builds in preallocation

system.time(tmp4<-unlist(lapply(1:100000,function(x){testfunction4help()}))) # 1.389 seconds

# but anything that uses lapply, could use mclapply

library(parallel)

system.time(tmp4<-unlist(mclapply(1:100000,function(x){testfunction4help()},mc.cores=4))) # 1.156 seconds


############################
############################
############################
############################


# Approach 4
## Byte compilation

library(compiler)

testfunction5<-cmpfun(testfunction3)

system.time(tmp5<-testfunction5(100000)) # 0.038 seconds




