## Created By: AKumar
## Date : 9/18/2014
## Version : 1:0
#######################################################################

## The following programming script is a R function which enables to 
## cache time-consuming computations. If the matrix variables are constant,
## then it makes sense to cache the value of inverse of the matrix one time
## and then reuse the computed inverse matrix again from the cache without
## wasting time to recompute

## "makeCacheMatrix" is a function to create a special "Matrix" that can cache
## its inverse value

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(z) m <<- z
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## "cacheSolve" function calculates the inverse of the special "matrix" created
## by "makeCacheMatrix" function. However, it first checks to see
## if the inverse matrix already exisit in cache (already computed).
## if the inverse is residing in cache then it skips the computation and picks
## up the value from cache, otherwise it will compute the inverse of the
## matrix

cacheSolve <- function(x = matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
