## The function makeCacheMatrix creates a Matrix object that will storage the inverse calculated by means of
## the cacheSolve function. This will prevent from calculating the inverse of the same matrix multiple times
## saving computational effort


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


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

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
