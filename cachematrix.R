## These functions below are part of the "R Programming" Coursera course. The
## functions allow us to cache potentially time consuming functions for later use.
## While this might not be particularly useful for small data sets, it can really
## decrease computation time for large data sets.
##
## Brian Groves, September 20th, 2014.

## This function creates a list in which we can store the value of the matrix and 
## also its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function checks to see if the inverse of a matrix has already been calculated
## If the inverse has not already been calculated, then it calculates the inverse and
## stores the data.

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
