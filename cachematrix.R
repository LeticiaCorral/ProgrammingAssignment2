## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The input for this function is a matrix object
## Four functions are defined
##  			set: to store the value of the matrix to the cache
##			get: to retrieve the value of the matrix from the cache
##			setInvMatrix: to store the inverse of the matrix to the cache
##			getInvMatrix: to retrieve the inverse of the matrix from the cache
##
## The output is a list object with the four functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInvMatrix <- function(inv) m <<- inv
  getInvMatrix <- function() m
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}


## Write a short comment describing this function

## Return a matrix that is the inverse of 'x'
## Gets the cached value if exists. Otherwise it will be null.
## If null, the function calculates the inverse with "solve" function and saves the value to cache


cacheSolve <- function(x, ...) {
  m <- x$getInvMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInvMatrix(m)
  m
}


