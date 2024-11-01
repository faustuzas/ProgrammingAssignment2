## Functions in this file provides a special matrix object which is able
## to cache some of the results.

## makeCacheMatrix creates an special matrix which is able to
## cache certain matrix operations if appropriate functions are used. 
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() { x }
  
  setinverse <- function(i) { inverse <<- i }
  
  getinverse <- function() { inverse }

  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve returns the inverse of the matrix by
## checking whether it was already computed and cached result
## can be returned, or by calculating the inverse on the spot.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    return(inv)
  }
  
  inv <- solve(x$get())
  x$setinverse(inv)
  
  inv
}
