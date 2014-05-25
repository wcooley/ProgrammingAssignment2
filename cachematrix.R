#
# Functions `makeCacheMatrix` and `cacheSolve`.
#
# These are so close to the examples `makeVector` and `cachemean` that it almost
# feels like cheating...

# The `makeCacheMatrix` function creates a list of closures, which access a
# stored matrix and an alternative representation of the matrix.
# Returns a list of functions which encapsulate the matrix object:
#  * getter & setter for the matrix
#  * getter & setter for the alternative representation

makeCacheMatrix <- function(x = matrix()) {
    invmatrix <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }

    get <- function() x

    setinverse <- function(inv) invmatrix <<- inv
    getinverse <- function() invmatrix

    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The `cacheSolve` function returns the cached inverse of the matrix inside the
# scope accessed by the functions in `x`, solving and caching the inverse if it
# does not already exist.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
