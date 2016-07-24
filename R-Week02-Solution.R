
# In probability, "n choose r" means n! / (( n - r)! * r!).
# For example, "5 choose 3" evaluates to 10.
# R has built in functions for choose() and factorial(). Your task is to write your own variation of the choose()
# function, that does not use either of these functions. 


# Calculate Factorial
calcFactorial <- function(a){
  b <- 1
  for (i in 1:a) {
    b <- b*i
  }
  return(b)
}

# Calculate Combination nCr using formula n!/((n-r)!*r!)
calCombination <-function(n,r){
  if (is.numeric(n) & is.numeric(r) & (n > 0) & (r >= 0) & (n > r)) {
  response <- calcFactorial(n)/((calcFactorial(n-r))*calcFactorial(r))
  }
  else {
    response <- "Invalid input"
  }
  return(response)
}

calCombination(4,2)
calCombination(5,4)
calCombination(5,6)
calCombination(5,-1)

