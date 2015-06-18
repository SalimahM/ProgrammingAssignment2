## Two functions for Assignment 2, i.e. makeCacheMatrix and cacheSolve.
## These functions allow you to cache the inverse of a matrix.
## This allows us to save some time rather than running the computation again.

## The first function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## - set the value of the matrix
  invrs <- NULL
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  ## - get the value of the matrix
  get <- function() x
  
  ## - set the value of the inverse of a matrix
  setinvrs <- function(inverse) invrs <<- inverse
  
  ## - get the value of the inverse of a matrix
  getinvrs <- function() invrs
  list(set = set,
       get = get,
       setinvrs = setinvrs,
       getinvrs = getinvrs)
  
}


## This function computes the inverse of the special matrix returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  ## If the inverse has been calculated, cacheSolve returns that cached inverse and saves processing time.
  ## Return a matrix that is the inverse of 'x'
  invrs <- x$getinvrs()
  if(!is.null(invrs)) {
    message("getting the cached data")
    return(invrs)
  }
  ## If it has not been created, then cacheSolve calculates the inverse and caches it.
  data <- x$get()
  invrs <- solve(data, ...)
  x$setinvrs(invrs)
  invrs
  
}