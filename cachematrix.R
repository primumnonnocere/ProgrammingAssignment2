## These functions are designed to cache matrix inversions, which are computationally expensive procedures, storing these values for later use.

## This first function is designed to produce a matrix array which can additionally cache its inversion.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solvematrix) inverse <<- solvematrix
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This second function is designed to compute the inverse of the matrix as produced by the previous function "makeCacheMatrix"

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}