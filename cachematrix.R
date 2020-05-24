## The following functions aim to solve and cache
## the inverse of the matrix that is passed to them

## makeCacheMatrix creates a special matrix that 
## can cache the given matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(invr) inv <<- invr
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve checks if the inverse of the matrix
## has been cached already, otherwise it solves
## for the inverse, caches it and returns it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}