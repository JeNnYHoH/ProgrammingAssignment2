## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {
    m <<- y
    inv <<- NULL
  }
  
  get <- function() m
  
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv  ##get the inverse
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(m, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- m$getInverse()
  
     if (!is.null(inv)) {
       message("getting cached data")
       return(inv)
  }
  
    mat <- m$get()           ##get the matrix
    inv <- solve(mat, ...)
      m$setInverse(inv)      ##inverse to the object
      inv
}
