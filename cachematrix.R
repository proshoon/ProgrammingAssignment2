## function makeCacheMatrix creates a matrix that caches its inverse 
## function cacheSolve creates an inverse of a matrix if it does not exist in cache

## returns a matrix containing functions to manipulate variables

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## function cacheSolve returns a matrix that is the inverse of the input matrix

cacheSolve <- function(x, ...) {
  
        ## checks if inverse is cached. if exists, returns that. else calculates
  
  
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve (data)
    x$setinverse(m)
    m
}
