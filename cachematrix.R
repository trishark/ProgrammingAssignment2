## This code stores a matrix and its inverse in a cached object. It also
## provides a method to retrieve the matrix inverse and calculate it.

## Creates an object with functions that can cache a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        xInverse <- NULL
        ## Set the matrix whose inverse is desired. If the new matrix matches
        ## the existing matrix, do nothing. Otherwise, set the inverse of the
        ## matrix back to NULL since it will need to be recalculated.
        set <- function(y) {
                if(y != x) {
                        x <<- y
                        xInverse <<- NULL
                }
        }
        ## Provide the ability to get the raw matrix
        get <- function() x
        ## Set the inverse value in the cache so that it doesn't have to be
        ## recalculated in the future (as long as there are no changes)
        setInverse <- function(inverse) xInverse <<- inverse
        ## Get the inverse of a matrix
        getInverse <- function() xInverse
        ## Returns a list of functions to store and retrieve values from the
        ## cache
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Calculates and returns the inverse of a matrix, pulling from the cache when available.

cacheSolve <- function(x, ...) {
        ## Get the matrix inverse from the cache matrix object
        xInverse <- x$getInverse()
        ## If the inverse of the matrix is not null, return it
        if(!is.null(xInverse)) {
             return(xInverse)   
        }
        ## Get the matrix in order to calculate the inverse
        data <- x$get()
        ## Calculate the inverse of the matrix since it was not in the cache.
        xInverse <- solve(x, ...)
        ## Store the matrix in the cache so it won't need to be calculated in the future
        ## (as long as the matrix data does not change)
        x$setInverse(xInverse)
        ## Return the matrix inverse as just calcuated
        xInverse
}
