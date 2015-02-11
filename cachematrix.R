## A pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    x <- NULL
    set <- function(b) {
        a <<- b
        x <<- NULL
    }
    get <- function() a
    setinversion <- function(solve) x <<- solve
    getinversion <- function() x
    list(set = set, get = get,
         setinversion = setinversion,
         getinversion = getinversion)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinversion()
    if(!is.null(m)) {
        message("getting cached matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinversion(m)
    m
}
