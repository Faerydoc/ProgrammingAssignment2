## A pair of functions that cache the inverse of a matrix.

## makeCacheMatrix creates a special "vector", which is  a list containing a function to
      ## set the value of the vector
      ## get the value of the vector
      ## set the value of the inverse matrix
      ## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      
      i <- NULL
      set <- function(y) {
      x <<- y
      i <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) i <<- solve
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}



## The following function calculates the inverse of the special "vector" created with 
## the above function. However, it first checks to see if this has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse matrix  and sets the value of the inverse matrix
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      cachemean <- function(x, ...) {
            i <- x$getinverse()
            if(!is.null(i)) {
                  message("getting cached data")
                  return(i)
            }
            data <- x$get()
            i <- solve(data, ...)
            i$setinverse(i)
            i
      }
}
