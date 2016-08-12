## makeCacheMatrix is a list of 4 functions that sets/gets the input arg and sets/gets the inverse of the input matrix
## cacheSolve solves the matrix if there is no cached inverse value or return the cached value if there is one

## both are sourced from the example code with proper modifications. 
## it has passed the test run on my own machine.

## In general I think this assignment is a bit weird compared to the lecture content. 
##I had to crack the example code by reading around on functional programming in R and some advanced topics
## http://adv-r.had.co.nz/Functions.html this url is helpful in understanding what is going on in the example code

## Very similar to the example but applied to matrix rather than vector
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse of the matrix
  inverse <- NULL
  
  ## set is the function that takes the value of the input arg and sets its inverse to NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
    
  }
  ## get simply passes the input arg
  get <- function() x
  
  ## set inverse sets the value of inverse in parent environment to the input arg inv
  setInverse <- function(inv) inverse <- inv
  
  ## getInverse gets the value of variable inverse
  getInverse <- function() inverse
  
  ## now returen the list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## fetch the cached inverse and store it to m
  m <- x$getInverse()
  
  ## now, check whether m is NULL (i.e. if there is a cached value)
  if(!is.null(m)){
    message("getting cached matrix inverse")
    return (m)
  }
  
  ## if m is NULL...
  ## first, pass the input matrix to data
  data <- x$get()
  ## calculate the inverse of data
  m <- solve(data, ...) 
  ## set the cache to the calculated inverse
  x$setInverse(m)
  ## return the inverse
  m
  
}

