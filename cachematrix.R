## the combination of both functions is intended to
## cache the inverse of a matrix to avoid repetitive calculations

## creates a matrix-like list for use with cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, 
       get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}


## returns the inverse of a matrix
## if the inverse of a matrix was already calculated,
## a cached value is returned

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
