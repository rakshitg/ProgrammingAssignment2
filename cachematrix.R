## Cacheing the Inverse of a Matrix

## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() {
    x
  }
  setInverse <- function(solve) {
    inverse <<- solve
  }
  getInverse <- function() {
    inverse
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}

##Test script
##a<-makeCacheMatrix()
##a$set(matrix(1:4,2,2))
##cacheSolve(a)
