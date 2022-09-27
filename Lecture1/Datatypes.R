
processexamplemat<-function(M){
  ## do this 100 times
  for(i in 1:100){
    ## Calculate row sums of the matrix
    Mrs<-apply(M,1,sum)
    ## Calculate variance of the log row sums
    var(log(Mrs))
  }  
  ## report something
  return("Done!")
}

#object.size(examplemat)/1000000
#system.time(processexamplemat(examplemat))

## Logical 

examplemat<-matrix(sample(c(T,F),1000000,replace=T),ncol=1000)
examplemat[1:4,1:4]
object.size(examplemat)/1000000
system.time(processexamplemat(examplemat))

## Integers

mode(examplemat)<-"integer"
examplemat[1:4,1:4]
object.size(examplemat)/1000000
system.time(processexamplemat(examplemat))

## Single precision

mode(examplemat)<-"single"
examplemat[1:4,1:4]
object.size(examplemat)/1000000
system.time(processexamplemat(examplemat))

## Double precision

mode(examplemat)<-"double"
examplemat[1:4,1:4]
object.size(examplemat)/1000000
system.time(processexamplemat(examplemat))

## Complex numbers

mode(examplemat)<-"complex"
examplemat[1:4,1:4]
object.size(examplemat)/1000000
system.time(processexamplemat(examplemat))

## Raw representation

mode(examplemat)<-"raw"
examplemat[1:4,1:4]
object.size(examplemat)/1000000
system.time(processexamplemat(examplemat))

