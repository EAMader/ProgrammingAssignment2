## RProgramming Assignment 2 - Elizabeth Mader
## This series of functions is designed to take an invertible matrix and calculate
## its inverse, caching the value once calculated to save time on future calls 
## for the matrix inverse.

## This function creates an enclosing enviroment holding a reference to a particular
## matrix, and the value (i) of the inverse of the matrix, if it has been calculated.
## If the inverse has not been calculated, i is NULL.  
## The output is a list containing four named functions "set", "get", "setinverse"
## "getinverse".  

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(matrix_inverse) i <<- matrix_inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function takes a list of four functions, named "set", "get", "setinverse"
## "getinverse", and returns the matrix that is the inverse of the matrix 'x' stored 
## in its enclosing environment. It is typically called on a particular 
## assignment of makeCacheMatrix.  If the inverse has already been caluclated it does
## not recalculate, but retrieves the stored value instead.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
