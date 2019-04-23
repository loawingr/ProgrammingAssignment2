## makeCacheMatrix() creates a special "matrix" object that can cache its inverse
## cacheSolve() computes the inverse of the special "matrix" returned by makeCacheMatrix().
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## Assumes that a square matrix is received as argument x
makeCacheMatrix <- function(x = matrix()) {
    ## upon construction clear inv environment variable
    inv <- NULL
    ## set() clears the cache & assigns the matrix to invert without input validation
    setMatrix <- function(y){
        x <<- y
        inv <<- NULL
    }
    ## getMatrix() returns the matrix that we will be attempting to invert
    getMatrix <- function() x
    ## cache the inverted matrix that was passed as an argument
    setInverse <- function(i) inv <<- i
    ## return the cached cached inverted matrix
    getInverse <- function() inv
    ## return a list of the 4 named functions
    list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve() calculates the inversion of the special "vector" created with the above function.
## However, it first checks to see if the inversion has already been calculated.
## If so, it gets the inversion from the cache and skips the computation.
## Otherwise, it calculates the inversion of the matrix and 
## sets the value of the inverted matrix in the cache via setInverse().

cacheSolve <- function(x, ...) {
    ## get the cache contents
    m <- x$getInverse()
    ## see what is in the cache
    if(!is.null(m)){
        message("getting matrix inverse from cache")
        return(m)
    }
    ## get the matrix we want to invert
    data <- x$getMatrix()
    ## calculate the matrix inversion
    m <- solve(data, ...)
    ## store the inverted matrix to the cache
    x$setInverse(m)
    ## return the inverted matrix
    m
}
