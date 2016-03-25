## makeCacheMatrix
## creates a list of functions to:
## set a matrix
## get a matrix
## set the cached inverse of a matrix
## get the cached inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <<- NULL
  ## set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## get the matrix
  get <- function() x
  
  ## set/get inverse matrix
  setInverse <- function(i) inv <<- i
  getInverse <- function() inv
  
  list(set = set, get = get, setInv = setInverse, getInv = getInverse)
}

##
## cacheSolve
## INPUT: matrix
## OUTPUT: inverse matrix

cacheSolve <- function(x, ...) {
  ## get the cached value and check if NULL
  inv <- x$getInv()
  if (!is.null(inv)) {
    message("returning cached inverse")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  inv
}
