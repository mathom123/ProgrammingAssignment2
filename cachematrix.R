## makeCacheMatrix and cacheSolve can be used to store a matrix and its inverse
## so that it doesn't need to be continually recalculated

## makeCacheMatrix is a function that takes a matrix and will create a 
## list of four functions where the argument matrix and its inverse are
## stored in a cache for quick access.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve is a function that will store a matrix
## and calculate and store its inverse as well or if a particular matrix and its
## inverse have already been stored it will return their values without needed
## to calculate them again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
        message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
