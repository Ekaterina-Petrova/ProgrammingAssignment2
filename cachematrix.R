## Following 2 functions will allow to keep the result
## of evaluation of the inverse matrix in cache since it
## was initially calculated untill the matrix was
## changed. Comment 'getting cached data' informs
## the user whether inverse calculation has been already done.

## makeCacheMatrix creates a special "matrix" - a list with set,
## get, setinverse, and getinverse methods.

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


## cacheSolve function calculates the inverse matrix
## or retrieve the result from the cache in case
## it was done for the same matrix before. Result
## of inverse calculation is transfered to makeCacheMatrix

cacheSolve <- function(x, ...) {
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

