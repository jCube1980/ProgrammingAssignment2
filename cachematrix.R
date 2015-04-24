
## A pair of functions that can be used to determine and cache matrix inverse

        # Author: jCube1980
        # Date: 4/23/2015


## Function defines a list of get/set functions that can be used to store and access a matrix and its inverse.

# Argument x - A matrix, ideally invertible.

makeCacheMatrix <- function(x = matrix()) {

        ## Variable to hold the inverse
        matInverse <- NULL

        ## Returns the matrix
        get <- function() x

        ## Sets a new matrix
        set <- function(y) {
                x <<- y
                matInverse <<- NULL
        }

        ## Returns the inverse
        getInverse <- function() {
                matInverse
        }

        ## Sets the inverse
        setInverse <- function(inv) {
                matInverse <<- inv
        }

        ## Returns a list of functions.
        list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}


## Function returns the matrix inverse from cache or after calculation.

# Argument x - A function of type makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Get the inverse from cache
        matInverse <- x$getInverse()

        ## If cache is not empty, return it, else calculate, cache and return
        if(!is.null(matInverse)) {
                message("Returning from cache...")
                return (matInverse)
        } else {
                matInverse <- solve(x$get(), ...)
                x$setInverse(matInverse)
                matInverse
        }
}
