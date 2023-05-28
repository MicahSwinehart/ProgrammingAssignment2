## These functions are used to solve the inverse of a matrix 
## and cache the resultant data to be quickly accessed at a
## later time. Useful for large matrices.

## Creates a list of functions on a matrix
## to be used in cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Solves inverse of matrix and caches result to be accessed
## quickly later

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
