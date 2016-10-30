## makeCacheMatrix makes an object of type makeCacheMatrix, while
## cacheSolve returns the value of the inverse and caches it.
## If it has been computed,cacheSolve takes the value from the cache.


## This function makes the object from a matrix and returns a list of 
## its behaviors 

makeCacheMatrix <- function(x = matrix) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(invSolve) inverse <<- invSolve
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This function checks if the inverse has been solved for the object and
## if yes returns it, if not solves it and the returns it

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}

# example 
# a <- matrix(c(1, 0, 5, 2, 1, 6, 3, 4, 0), 3, 3)
# temp2 <- makeCacheMatrix(a)
# cacheSolve(temp2)
# cacheSolve(temp2)