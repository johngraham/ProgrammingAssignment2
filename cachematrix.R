
## These functions allow for the caching of "expensive" computations like 
## the matrix inverse.


## makeCacheMatrix takes a regular matrix and turns it into a "super matrix"
## that stores a matrix and it's inverse result together.
## The matrix is stored as x
## The matrix inverse is stored as inv, set to NULL until the operation is made
## There are 4 accessor functions to get and set the values.

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
       x <<- y
       inv <<- NULL
     }
     get <- function() x
     setinverse <- function(inver) inv <<- inver
     getinverse <- function() inv
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## Returns inverse of matrix "x" from solve()
## This function is very similar to cachemean example

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'  
        inv <- x$getinverse()
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        
        # and if *not* cached, compute inverse and save it
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}