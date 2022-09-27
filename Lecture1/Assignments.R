## Assigning values within a loop

a<-5
b<-15

for(a in 1:9){
  b<-sqrt(a)
}


c(a,b)


## assigning values within a function

a<-5
b<-15

testfun<-function(a){
for(a in 1:9){b<-sqrt(a)}
}
testfun(a)



## assigning values within a function part ii

a<-5
b<-15

testfun<-function(a){
  for(a in 1:9){b<<-sqrt(a)}
}
testfun(a)

c(a,b)


## assigning values within a function part iii

a<-4
b<-2

testfun<-function(a){
  for(a in 1:9){
    b<<-sqrt(a)
  }
}
testfun(a)

c(a,b)




## chained assignments

a<-4
b<-2

a <- b <- 15


c(a,b)


## chained assignments

a<-4
b<-2

a = b <- 5

c(a,b)

a <- b = 15

c(a,b)

2*(b=15)


## Protected values

# don't do this.


a<-6
b<-6
c<-16

6 ->> c

## Protected values

# don't do this.

a<-6
b<-6
d<-16

6 ->> d

## Two directions

a<-6
b<-6
d<-6


a <- 5 -> b ->> d

c(a, b, d)



## Function arguments

n<-5
N<-5

rnorm(n = 10, mean = 0, sd = 1)
rnorm(n <- 10, mean = 0, sd = 1)
rnorm(n <<- 10, mean = 0, sd = 1)
rnorm(n == 10, mean = 0, sd = 1)
rnorm(N = 10, mean = 0, sd = 1)
rnorm(N <- 10, mean = 0, sd = 1)
rnorm(N <<- 10, mean = 0, sd = 1)
rnorm(N == 10, mean = 0, sd = 1)








