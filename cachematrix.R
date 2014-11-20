## The functions below will cache the inverse of a matrix rather than 
##  compute it repeatedly

## The makeCacheMatrix function creates a special "matrix" object that 
##  can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

  ## i is the cached value of the inverse
  ## x is the current matrix whose inverse is being evaluated
  
  i <- NULL 
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the special "matrix" 
##  returned by makeCacheMatrix function above
cacheSolve <- function(x, ...) {
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
    

