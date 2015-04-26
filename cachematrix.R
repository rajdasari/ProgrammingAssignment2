# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.
# Creating a matrix wrapper, that allows for caching the matrix inversion.
# x: The matrix wrapped.
# Returned: Is a list representing the special matrix, containing functions to
#get the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
cached.inverse <- NULL
# While setting new values to the matrix, the cached inverse are to be reset
set <- function(y) {
  x <<- y
  cached.inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) cached.inverse <<- inverse
  getinverse <- function() cached.inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
# The following Calculates the inverse of the matrix created with the function makeCacheMatrix.
# It checks to see if the inverse has already been calculated. If so, it gets
# the inverse matrix from the cache. Otherwise, it calculates the inverse of
# the matrix and sets the value of the inverse in the cache.
cacheSolve <- function(x, ...) {
inverse <- x$getinverse()
#If the inverse is already calculated it has to be returned
  if(!is.null(inverse)) {
    message("Getting Cached Data")
} else {
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
  }
  inverse
}