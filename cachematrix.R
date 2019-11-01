## Put comments here that give an overall description of what your
## functions do

# There're 2 functions in this script, makeCacheMatrix & cacheSolve,
# which basically computes the inverse matrix during cold-start 
# and caches the inverse. 
# When the same matrix has been retrieved, it will return
# the inverse matrix without re-computing it.
# The benefit is to save unnessary computation cost,
# beause computing inverse can be very expensive
# for some large matrices.

## Write a short comment describing this function
# This function creates a list of function including
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of its inverse matrix
# 4. get the value of its inverse matrix
makeCacheMatrix <- function(x = matrix()) {

  my_matrix <<- x
  m <- NULL
  set <- function(y) {
    my_matrix <<- y
    m <<- NULL
  }
  get <- function() my_matrix
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
