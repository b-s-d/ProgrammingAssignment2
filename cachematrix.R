## The Functions below provide functionalities to compute and cache the inverse of a matrix
 

## Creates a "matrix" object capable of caching its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  ## Sets the matrix object
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  
  ## Gets the matrix object
  get <- function() x
  
  ## Sets the matrix 
  setinverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Gets the inverse of the matrix object
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  

}


## Computes the inverse of the "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  
  i <- x$getinverse()
  
  ## If the inverse has already been calculated (and the matrix has not changed) 
  ## then the cachesolve should retrieve the inverse from the cache.
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
}
