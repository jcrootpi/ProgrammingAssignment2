## This creates a framework for 'cacheing' potentiall expensive computes.
## e.g., if your loop inverts a (large) matrix repeatedly, if its NOT changing,
## No need to re-compute inverse, use a copy of the inverted Matrix instead

## This makes a 'cacheable' matrix, as far as I can tell.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This checks for cache of inverse of provided matrix, returns cache or computes inverse and caches result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if (!is.null(m)){
            message("Getting cached inverse")
            return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
