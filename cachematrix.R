# The pair of functions makeCacheMatrix and cacheSolve allow to cache the inverse of a matrix rather than computing it repeatedly 
# Example of usage:
# > x <- matrix(1:4,2,2)
# > s <- makeCacheMatrix(x)
# > cacheSolve(s)

# This function creates a special "matrix" .
# makeVector creates a special "matrix" (that can cache its inverse) 
# which is really a list containing a function to:
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse of the matrix
# get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix
# It first checks to see if the inverse matrix has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
