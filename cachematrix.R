## These functions allow the user to cache the inverse of a matrix
## so that the computation needs only to happen once, but can be used repeatedly. 

## The first function creates the matrix and the functions that allow the user
## to get and set the data, as well as its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The second function returns the inverse of a matrix created by the first function.
## If this value has not been previously computed, then it computes it and caches it. 
## Otherwise it returns the cached inverse.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached inverse")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
