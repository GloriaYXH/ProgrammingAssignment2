## A pair of functions that cache the inverse of a matrix.


## This function serves as a "cache" of a matrix. 
## It creates a list of callable functions to 
## set, get, setinverse and getinverse on a matrix.

makeCacheMatrix <- function(x=matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of a matrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then it retrieves the inverse from the cache.

cacheSolve <- function(x,...) {
  m <- x$getinverse()
  if(!is.null(m)) {      ## Get inverse from cache if it's already calculated.
    message("getting cached data")
    return(m)  
  }
  data <- x$get()
  m <- solve(data, ...)   ## calculate the inverse of matrix.
  x$setinverse(m)
  m
}