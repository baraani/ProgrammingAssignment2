## Second programming assignment for Coursera R Programming

## Here We compute and cache the Matrix inversion of a given matrix


## makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the Matrix inversion
## get the value of the Matrix inversion

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The following function calculates the mean of the special "matrix" 
## created with the above function. 
## It first checks to see if the Matrix inversion has already been calculated.
## If so, it gets the Matrix inversion from the cache and skips the computation.
## Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setsolve function

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}