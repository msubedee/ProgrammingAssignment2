## The goal of these two functions is to calculate the inverse of a matrix. The inverse of a matrix will be 
## returned right away if the matrix has been cached before, so the same computation is not done repeatedly.

## The makeCacheMatrix function creates a special "matrix" object that cache its inverse. It is really a 
## list containing a function to:
##
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the inverse of the matrix
##      4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The cacheSolve function computes the inverse of the special "matrix" that is returned fromh makeCacheMatrix 
## function. This function checks if the inverse of the matrix has already been calculated. In this case, it
## skips the computation and retrieve the inverse from the cache. In other case, it computes the inverse and 
## sets the inverse matrix in the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)){
        message("Getting cached data")
        return (m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    return (m)       
}
