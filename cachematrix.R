# This is my solution
# Matrix inversion is a costly computation and there may be some benefit to
# caching the inverse of a matrix rather than compte it repeatedly.
# The following pair of functions are used to create a special object
# that stores a matrix and caches its inverse. 

# This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

# The following function computes the inverse of the special "matrix" created by
# makeCacheMatrix. If the inverse has already been calculated, then it can 
# retireve the inverse from the cache. 
# Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
