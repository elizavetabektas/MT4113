
################################### Exercise 1 #################################################

# It was a logical error. The algorithm was evaluating i:i first, then did i+k-1, which meant only 1 element was considered at a time

################################### Exercise 2 #################################################

findRuns <- function(x, k) {
  n <- length(x)
  r <- NULL
  for (i in 1:(n - k + 1)) {
    if (all(x[i:(i + k - 1)] == 1)) {
      r <- c(r, i)
    }
  }
  return(r)
}

testCase <- c(1, 0, 0, 1, 1, 1, 0, 1, 1)
findRuns(testCase, 2)

#################################### Exercise 4 #################################################

# the number of loops increased, but I cannot see how to reduce them (for now)

findRuns2 <- function(x, k){
  
  # create vector of positions at which a new run of zeros and ones begins
  zeros_ones <- c(0, which(diff(x) == 1 | diff(x) == -1)) + 1
  zeros_ones2 <- c(zeros_ones, length(x) + 1)
  
  # find length of each run
  lengths <- rep(NA, times = length(zeros_ones))
  for (i in 1:(length(zeros_ones))){
    lengths[i] <- zeros_ones2[i+1] - zeros_ones[i]
  }
  
  # find list of values of k or greater and find their lengths
  list_of_k_or_greater <- c()
  their_lengths <- c()
  for (i in which(lengths >= k)) {
    if (x[zeros_ones[i]] == 1){
      list_of_k_or_greater <- c(list_of_k_or_greater, zeros_ones[i])
      their_lengths <- c(their_lengths, lengths[i])
    }
  }
  
  # iterate through all of them and find each location
  where_k_1s <- c()
  for (i in 1:length(their_lengths)){
    if (their_lengths[i] == k) {
      where_k_1s <- c(where_k_1s, list_of_k_or_greater[i])
    }
    if (their_lengths[i] > k) {
      for (j in list_of_k_or_greater[i]:(list_of_k_or_greater[i] + their_lengths[i] - 2))
        where_k_1s <- c(where_k_1s, j)
    }
  }
  return(where_k_1s)
}
findRuns2(testCase, 2)

# One possible approach would be to do the following:
  
# 1. Use the which() and diff() commands to create a vector of the positions at which a new run of zeros and ones begins.
# Don’t forget position 1.
# 2. Create a second vector that is identical, but has the value length(x)+1 appended to it
# 3. Now determine the lengths of those runs of zeros and ones, using the vector you have just generated as a starting point.
# 4. Identify the locations that are the starts of runs of length  k or greater and take the value of 1. Record their lengths.
# 5. Process the vector of run starts to return multiple values for any run of length greater than k
# 6. Generate a random binary sequence of length 100000 on which to test this

library(microbenchmark)

microbenchmark(findRuns(testCase, 2)) # mean: 12.21157 microseconds

microbenchmark(findRuns2(testCase, 2)) # mean: 340.677 microseconds

# need to test this on longer binary sequences

