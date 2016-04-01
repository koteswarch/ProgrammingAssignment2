## Compute inverse of a new matrix and store it in a cache.
## Obtain the inverse of an old matrix from the cache. 

## Put the matrix passed as an argument in the cache. 
## Generate functions to 
## (a) fetch this matrix from the cache if an inverse is not yet computed
## (b) put the inverse computed in the cache and
## (c) fetch the inverse (if already computed) from the cache

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inver) inv <<- inver
    
    getInverse <- function() inv
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Return the inverse of a matrix (if already computed)
## Else, compute the inverse and put it in the cache.

cacheSolve <- function(x, ...) {
        
    inv <- x$getInverse()
    
    if(!is.null(inv)) {
        message("Getting cached inverse!")
        return(inv)
    }
    
    data <- x$get()
    
    inv <- solve(data, ...)
    
    x$setInverse(inv)
    
    inv
}
