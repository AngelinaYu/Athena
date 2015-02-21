## A pair of functions that cache the inverse of a matrix
## 

## makeCacheMatrix:This function creates a special
## "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL

      # 1st function to set the matrix
      set <- function(y) {
         x <<- y
         inv <<- NULL
      }

      # 2nd function to get the matrix
      get <- function( ) x

      # 3rd function to set the inverse of the matrix
      setinv <- function(inverse) inv <<- inverse

      # 4th function to get the inverse of the matrix
      getinv <- function( ) inv

      # return above 4 functions as a list
      list ( set = set, get = get, setinv = setinv, getinv =getinv)
}


## cacheSolve :This function computers the inverse of the special
## "matrix" returned by makeCache Matrix above. If the inverse has
## already been calculated ( and the matrix has not changed), then
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv( )

    # if the inversed is called, return it 
    if(! is.null(inv) {
      message("getting cached data")
      return(inv)
    }

    # if no inverse is cached, calculate , cache and return it
    message("calculating data to cache")
    data <- x$get( )
    inv <- solve(data)
    x$setinv(inv)
    inv
}
