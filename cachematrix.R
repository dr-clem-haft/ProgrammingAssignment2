## This script computes the inverse of a matrix and stores it into cache.
## If the inverse of the same matrix is requested, this value is taken 
## from cache rather than having to recalulated the inverse.

## The following function makeCacheMatrix takes the matrix as an input
## and returns a list of functions to get and set inputs and outputs to 
## recalulate the inverse.

makeCacheMatrix <- function(x = matrix()) {
  # m, the value of the inverse is set to NULL at the start
  m <- NULL
  
  ## in case the matrix needs to be recalculated, x is reset with the
  ## new matrix and m is cleared
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## get is used to retrieve the matrix
  get <- function() x
  ## setinverse sets the value of m
  setinverse <- function(inverse) m <<- inverse
  ## getinverse retrieves the value of the inverse m
  getinverse <- function() m
  ## the following creates a list of the function and names them
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  }


## The following function cacheSolve takes the list output of the above
## function makeCacheMatrix as an input (a list) and assess if the 
## matrix is new then either retrieves the value of the inverse or
## recalculates it before returning it.

cacheSolve <- function(x, ...) {
  
  ## try and get the value for the inverse
  m <- x$getinverse()
  ## if an inverse has already been calulated return it and end function
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## if the inverse is not present then calcuate it by
  ## a. getting the data
  data <- x$get()
  ## b. using the solve function to compute the inverse
  m <- solve(data, ...)
  ## c. updating the value of the inverse
  x$setinverse(m)
  ## finally the inverse is returned
  m
}
