## Programming assignment #2
## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than computing it repeatedly 
## This assignment is to write a pair of functions that cache the inverse of a matrix.


##The first function, makeCacheMatrix creates a special "matrix", 
##set the value of the matrix
##get the value of the matrix
##set the matrix inverse
##get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    invMat <- NULL
    set <- function(y) {
        x <<- y
        invMat <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) invMat <<- inverse
    getInverse <- function() invMat
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The following function calculates the inverse of the special "matrix" 
## created with the above function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invMat <- x$getInverse()
    if (!is.null(invMat)) {
        message("getting cached data")
        return(invMat)
    }
    mat <- x$get()
    invMat <- solve(mat, ...)
    x$setInverse(invMat)
    invMat
}
