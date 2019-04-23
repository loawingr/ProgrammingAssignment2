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
    list(set = set, get = get, setInverse = setInverse, getMean = getMean)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
