## makeCacheMatrix is a function that returns a list of functions
## Its utility is to store a martix and a cached value of the 
## inverse of the same matrix.

## This function creates a special "matrix" object that 
## can cache its inverse. It returns a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
## this list is used as the input to cacheSolve().

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        ## use `<<-` to assign a value to an object in an environment 
        ## different from the current environment.
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
                
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## cacheSolve calculates the inverse of a "special" matrix 
## created with makeCacheMatrix

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        
        ## if the inverse has already been calculated
        if (!is.null(inv)) {
                ## get it from the cache and skips the computation.
                message("getting cached data")
                return(inv)
        }
        
        mat <- x$get()
        inv <- solve(mat, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setInverse(inv)
        
        inv
}
