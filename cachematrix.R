## Set of functions to create and retrieve a special matrix object that can
## store it's inverse.

## object returned from makeCacheMatrix is a list of funtions to set and get
## the matrix, as well as functions to set and get the inverse

makeCacheMatrix <- function(x = matrix()) {
    # stored inverse
    a <- NULL
    # set matrix
    set <- function(y) {
        x <<- y
        a <<- NULL
    }
    # get matrix
    get <- function() x
    # set inverse
    setInverse <- function(computedInverse) a <<- computedInverse
    # get inverse
    getInverse <- function() a
    # return list
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve computes and stores the inverse of a CacheMatrix, or return
## the previously computed result if available.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # get inverse
    a <- x$getInverse()
    # if not empty, use cached version
    if(!is.null(a)) {
        message("getting cached data")
        return(a)
    }
    # otherwise, compute inverse and store
    data <- x$get()
    a <- solve(data, ...)
    x$setInverse(a)
    a
    
}
