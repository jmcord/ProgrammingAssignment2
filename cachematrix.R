## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

A = matrix( 
c(2, 4, 3, 1, 5, 7,1,3,4), # the data elements 
nrow=3,              # number of rows 
ncol=3,              # number of columns 
byrow = TRUE)        # fill matrix by rows 

makeCacheMatrix <- function(x = numeric()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

B <- makeCacheMatrix(A)
cacheSolve(B)
