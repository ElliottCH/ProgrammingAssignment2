# Following 2 functions below are responsible for computing the inverse of the matrix as well as storing the computed value

##
# Caching a Matrix inverse value.
# Takes a matrix as an Arg, ex: matrix(rnorm(25), 5)
# Returns a vector of set, get, setInverse, getInverse
# Example calls 
# 1- cache <- makeCacheMatrix(matrix(rnorm(25), 5))
# 2- cache$get() --- Returns the original matrix 
##

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


##
# Computing the actual value of the inverse of a matrix to be cached, or retrieve a previously stored value
# Example calls 
# cache <- makeCacheMatrix(matrix(rnorm(25), 5))
# 1- cache$setInverse(matrix(rnorm(9), 3)) -- overrides previous cache
# 2- cache$get() -- Returns original matrix
# 3-  cacheSolve(cache) // computes matrix inverse or outputs cached data
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  inv1 <- solve(data)
  x$setInverse(data)
  m
}
