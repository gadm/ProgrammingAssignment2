## Creates a special matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse property
  inv <- NULL
  
  ## The matrix setter method
  set <- function(mat) {
    x <<- mat
    inv <<- NULL
  }
  
  ## The matrix getter method
  get <- function() x
  
  ## The inverse matrix setter method
  setInverse <- function(inverse) inv <<- inverse
  
  ## The inverse matrix getter method
  getInverse <- function() inv
  
  ## A list of all the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

makeCacheMatrix <- function(x = matrix()) {

}

## Computes the inverse of a special matrix which can cache its inverse (created using makeCacheMatrix).
## Returns the cached inverse matrix if available otherwise it calculates it
cachesolve <- function(x, ...) {
  ## Get the inverse matrix
  m <- x$getInverse()
  
  ## If the inverse has already been calculated, return it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the original matrix
  data <- x$get()
  
  ## Calculate the inverse matrix
  m <- solve(data, ...)
  
  ## Set the inverse matrix
  x$setInverse(m)
  
  ## Return the inverse matrix
  m
}