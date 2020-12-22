## R programming assignment 2
## Cacheing the inverse of a matrix using lexical scoping
## Creates a matrix with getters and setters

makeCacheMatrix <- function(x = matrix()) {
    # initialize the inverse to NULL
    inv <- NULL
    
    # setter for matrix
    set <- function(y) {
          x <<- y
        inv <<- NULL
}
    # getter for matrix
    get <- function() x
    
    # setter for the inverse
    set.inverse <- function(setinv) inv <<- setinv
    
    # getter for the inverse
    get.inverse <- function() inv
    
    # returns a list of getter and setter functions
    list(set = set, get = get,
         set.inverse = set.inverse,
         get.inverse = get.inverse)
}


## Cache solves the inverse of a matrix

cacheSolve <- function(cached.x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- cached.x$get.inverse()
        # if it exists return it
        if(!is.null(inv)) {
            message("getting cached inverse")
            return(inv)
        }
        # else calculate, store and return it
        raw.x <- cached.x$get()
        inv <- solve(raw.x, ...)
        cached.x$set.inverse(inv)
        
        # returns the inverse
        inv
}
