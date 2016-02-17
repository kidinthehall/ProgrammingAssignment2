## Matrix inversion is usually a costly computation and there may be some benefit to
## caching the inverse of a matrix rather than compute it repeatedly.

## The following functions will set matrix values, solve and cache for later use.

# makeCacheMatrix creates a matrix containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the solve of the matrix
# 4. get the value of the solve of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## cacheSolve will return the inverse of the matrix, using 
## the solve() function and places the result in cache.  If
## the matrix has already been calculated, the function will
## recall the cache and skip the solve() function.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}