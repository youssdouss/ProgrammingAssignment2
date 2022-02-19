## this function cache a function to ear time instead to compute every time


## The first function, makeCacheMatrix creates a special "matrix",

makeCacheMatrix <- function(x = matrix()) {
     makeCacheMatrix <- function(x = matrix()) {
          m <- NULL
          set <- function(y) {
               x <<- y
               m <<- NULL
          }
          get <- function() x
          setinverse <- function(inverse) m <<- inverse
          getinverse <- function() m
          list(set = set,
               get = get,
               setinverse = setinverse,
               getinverse = getinverse)
     }

}


## This function computes the inverse of the special “matrix” returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
     i <- x$getinverse()
     if (!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     data <- x$get()
     i <- solve(data, ...)
     x$setinverse(i)
     i
        ## Return a matrix that is the inverse of 'x'
}
