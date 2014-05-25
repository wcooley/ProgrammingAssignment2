#
# Functions `makeCacheMatrix` and `cacheSolve`.
#
# These are so close to the examples `makeVector` and `cachemean` that it almost
# feels like cheating...

# The `makeCacheMatrix` function creates a list of closures, which access a
# stored matrix and an alternative representation of the matrix.
#
# Returns a list of functions which encapsulate the matrix object:
#  * getter & setter for the matrix
#  * getter & setter for the alternative representation

makeCacheMatrix <- function(x = matrix()) {
    # Initialize the matrix alternative
    altx <- NULL

    # setter, which sets the `x` and `altx` in the parent scope
    set <- function(y) {
        x <<- y
        # If altx is already set (non-NULL), then we're invalidating the cache;
        # otherwise we're just initializing it.
        if (!is.null(altx)) message("invalidating cache")
        set_alt(NULL)
    }

    # getter for `x` in the parent scope
    get <- function() x

    # setter for `altx` in the parent scope
    set_alt <- function(alt) {
        altx <<- alt
    }

    # getter for `altx` in the parent scope
    get_alt <- function() altx

    list(set=set, get=get, set_alt=set_alt, get_alt=get_alt)
}

# The `cacheSolve` function returns the cached inverse of the matrix inside the
# scope accessed by the functions in `x`, solving and caching the inverse if it
# does not already exist.

cacheSolve <- function(x, ...) {

    # `m` is NULL if the cache is empty
    m <- x$get_alt()
    if (is.null(m)) {
        data <- x$get()
        m <- solve(data, ...)
        x$set_alt(m)
    }
    else {
        message("getting cached data")
    }
    m
}
