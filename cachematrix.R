## Functions that cache the inverse of a matrix

## Function that creates an object that can cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  +   m <- NULL
  +   set <- function(y) {
  +     x <<- y
  +     m <<- NULL
  +   }
  +   get <- function() x
  +   setinverse <- function(solve) m <<- solve
  +   getinverse <- function() m
  +   list(set = set, get = get,
  +        setinverse = setinverse,
  +        getinverse = getinverse)
}


## Function that computes the inverse of the matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  +   m <- x$getinverse()
  +   if(!is.null(m)) {
  +     message("getting cached data")
  +     return(m)
  +   }
  +   data <- x$get()
  +   m <- solve(data, ...)
  +   x$setinverse(m)
  +   m

}
