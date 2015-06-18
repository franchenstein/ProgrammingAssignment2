## The following functions are used to speed up the process of matrix inversion.
## A special matrix object will be created in the first function. This object
## can store its own inverse. The second function will return the inverse of the
## input matrix. It will try using the cached version first in order to save 
## time. All inputs are assumed to be invertible.

## Creates matrix object that can store its own inverse:

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inverse) inv <<- inverse
    
    getInverse <- function() inv
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Returns matrix inverse, trying cached version first.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv))
        {
            message("Getting cached data")
            return(inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv        
}
