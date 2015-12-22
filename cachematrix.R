## Put comments here that give an overall description of what your
## functions do

# The below functions are used to cache the inverse of a matrix so 
# that the cached value can be returned when the matrix has not 
# changed.

## Write a short comment describing this function

# This function creates a list for maintaining a special matrix 
# whose inverse will be cached. "inv" is the cache for the inverse,
# "x" is the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {    # Change the matrix
        x <<- y
        inv <<- NULL  # Cache is cleared since the matrix has changed
    }
    
    get <- function() x     # Returns the current matrix
    setinv <- function(invX) inv <<- invX  # Set the inverse
    getinv <- function() inv # Returns the inverse
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

# This function calculates the inverse of the special matrix created
# using makeCacheMatrix. If inverse is cached then it returns the
# inverse directly. Otherwise, it uses "solve" function to calculate
# the inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    if(!is.list(x)) {  # Check if the argument is special matrix
        message("Please use makeCacheMatrix before using this function")
    }
    
    inv <- x$getinv()
    
    if(!is.null(inv)) {  # Inverse is already calculated
        message("Getting cached data")
        return(inv)
    }
    
    matX <- x$get()
    inv <- solve(matX)  # Calculate the inverse if it is not already calculated
    x$setinv(inv)
    inv
}
