## CacheMatrix - These functions are here to cache the result of a matrix inversion
## because it is costly to inverse a matrix.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## It returns functions (setter/getter) to manipulate the inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL ## Making a new empty cache for the matrix inverse.
    
    ## setter for the data (non inverted matrix).
    ## when used it reset the cache to NULL (in case the data changed).
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## getter for the data
    get <- function() x
    
    ## setter for the matrix inverse.
    setinv <- function(invMatrix) m <<- invMatrix
    ## getter for the matrix inverse.
    getinv <- function() m
    
    ## We return the list of functions to manipulate the cached data.
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve is a helper that deal with cacheMatrix to compute matrix inverse
## only if needed. If no matrix inverse is in the cache it will compute it.
cacheSolve <- function(x, ...) {
    ## Checking the cache to see if the matrix inverse is already stored.
    m <- x$getinv()
    if(!is.null(m)) {
        ## Returning the data from the cache with message
        message("getting cached data")
        return(m)
    }
    ## Getting the matrix to be inversed
    data <- x$get()
    ## Computing the inverse matrix of 'x'
    m <- solve(data, ...)
    ## Storing the result matrix inverse in the cache
    x$setinv(m)
    
    ## Return a matrix that is the inverse of 'x'
    m
}
