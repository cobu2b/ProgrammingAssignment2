## =========================================================================
## In sum, this pair of functions is for caching an inverse of given matrix
## to avoid repeatedly computing.
## 
## Assume that the matrix supplied is always invertible.
## =========================================================================

## This function creates a special "matrix" object that can cache its inverse.
## @param my_matrix     Original matrix object
## @return              A list of set, get, setInverse, getInverse methods
makeCacheMatrix <- function(my_matrix = matrix()) {
    ## Initialize inverse of my_matrix
    inverse <- NULL
    
    ## Prepare methods
    set <- function(new_matrix) {
        my_matrix <<- new_matrix
        inverse <<- NULL
    }
    get <- function() my_matrix
    setInverse <- function(value) inverse <<- value
    getInverse <- function() inverse
    
    ## Return a list of prepared methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
## This function is for calculating, cachaing, and returning an inverse of
## matrix from the object created by makeCacheMatrix function
## @param  x    The object created by makeCacheMatrix function
## @param  ...  The arguments of solve function
cacheSolve <- function(x, ...) {
    ## Get cached inverse of given matrix
    inverse <- x$getInverse()
    
    if(!is.null(inverse)) {
        message("Getting cached data")
        return(inverse)
    }
    
    ## Compute inverse of given matrix
    data <- x$get()
    inverse <- solve(data, ...)
    
    ## Cache computed inversed of given matrix
    x$setInverse(inverse)

    ## Return a matrix that is the inverse of 'x'
    return(inverse)
}
