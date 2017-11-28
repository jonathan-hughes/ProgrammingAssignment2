## Caching the inverse of a matrix
## 
## These functions will create the inverse of a matrix when called for the first 
## time. The inverse matrix will be stored in a cache and subsequent calls will 
## return the cached data.

## This function will create a matrix object that can be used by the cacheSolve
## function to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    ## We'll follow the example given... 
    ## Create a list containing four functions
    ## Set the matrix
    ## Get the matrix
    ## Set the inverse matrix
    ## Get the inverse matrix
    
    ## Initially set a matrix to NULL
    m <- NULL
    
    ## Create the set function
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## Create the get function
    get <- function() x
    
    ## Create the setInverse function
    setInverse <- function(inverse) m <<- inverse
    
    ## Create the getInverse function
    getInverse <- function() m
    
    ## Create list of functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## This function will compute the inverse of a matrix object and store it in a 
## cache if it has not already done so. If the inverse of the matrix object is 
## in the cache it will return it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## We'll follow the example given...
    ## Call the getInverse function
    m <- x$getInverse()
    
    ## If the call is successful then we have already created the inverse and 
    ## retrieved it from the cache.
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## Not found so we need to create the inverse
    ## Get the matrix
    data <- x$get()
    
    ## Call the solve function to compute the inverse
    m <- solve(data, ...)
    
    ## Call the setInverse function to set the inverted matrix
    x$setInverse(m)
    
    ## Finally, return the inverted matrix
    m
}
