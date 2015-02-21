## The functions use lexical scoping features 
## for caching the inverse of an input matrix.
## The inverse will be retuned from cache if the input matrix has not changed.

## This function list all the available functions for setting the matrix and inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        if(!identical(data,y)){
            data <<- y
            inverse <<- NULL
        }
    }
    get <- function() data
    setMatrix <- function(inv) inverse <<- inv
    getMatrix <- function() inverse
    list(set = set, get = get,
         setMatrix = setMatrix,
         getMatrix = getMatrix)
}


## This function returns the matrix inverse using the caching features from makeCacheMatrix function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getMatrix()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setMatrix(inverse)
    return(inverse)   
}
