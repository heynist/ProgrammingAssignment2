## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix : takes a matrix and wraps it in a list of 4 functions
## cacheSolve : takes the output list of the makeCacheMatrix function
## and returns the inverse of the original matrix (using a cached value of it when possible)

## Write a short comment describing this function
## This function takes a matrix
## and constructs a list of 4 functions
## to get and set the matrix,
## and to get and set its inverse (which is cached locally)
makeCacheMatrix <- function(x = matrix()) {
  ## initialize the cache for the inverse of the matrix
  i <- NULL
  
  ## explicit call to set the matrix (consequently we need to reset the cached value for the inverse of it)
  setMatrix <- function(matrix) {
    x <<- matrix
    i <<- NULL
  }
  
  ## return the original matrix
  getMatrix <- function() x
  
  ## cache the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## return the inverted matrix
  getInverse <- function() i
  
  ## list the 4 functions
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Write a short comment describing this function
## This function checks to see whether the cached inverse matrix
## is available, and returns it if its is.
## If it is not available yet, it had it calculated (and cached for future reference)
## and finally returns it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## lookup the inverse in our chache
  i <- x$getInverse()
  
  ## if found, return it
  if(!is.null(i)) {
    message("getting cached data")
    return (i)
  }
  
  ## else, invert the given matrix
  i <- solve(x$getMatrix())

  ## cache it
  x$setInverse(i)

  ## and return it
  i
}
