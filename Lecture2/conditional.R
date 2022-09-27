

#switch()

## see also ifelse, next and some tidyverse functions

## from the help file

## treating data differently based on argument
## could also treat data differently depending on the data

require(stats)
centre <- function(x, type) {
  switch(type,
         mean = mean(x),
         median = median(x),
         trimmed = mean(x, trim = .1))
}

x<-rexp(100,4)

centre(x, "mean")
centre(x, "median")
centre(x, "trimmed")



### Note that 

centre(x)


require(stats)
centre <- function(x, type = 'mean') {
  switch(type,
         mean = mean(x),
         median = median(x),
         trimmed = mean(x, trim = .1))
}

x<-rexp(100,4)

centre(x)
centre(x, "median")



