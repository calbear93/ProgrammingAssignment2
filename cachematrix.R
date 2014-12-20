## The following set of functions can be used to calculate, store (cache) and retrieve ...
## ... the inverse of a square invertible matrix.

## This is the first function which creates a special object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) { ## Signalling that the input will be a matrix
     inv <- NULL                            ## Inverse reset to NULL everytime function is called
     set <- function(y) {                   ## Can be used to assign a new input matrix to ...
          x <<- y                           ## ... makeCacheMatrix without directly calling the function
          inv <<- NULL
     }
     ## The get, setinverse and getinverse functions are defined here, ...
     ## ... but will be accessed when the cacheSolve function is run.
     get <- function() {x}                              ## Simply returns the original matrix input
     setinverse <- function(inverse) {inv <<- inverse}  ## Stores the value of the inverse using superassignment
     getinverse <- function() {inv}                     ## Returns cached value
     ## Now to create a list of the internal functions to enable a calling function to access the functions.
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The second function computes the inverse of the special object returned by the previous function ...
## ... but if the inverse has already been calculated, cacheSolve retrieves the cached inverse.
cacheSolve <- function(x, ...) {           ## It takes as the input the object created by makeCacheMatrix
     inv <- x$getinverse()                 ## Retrieves the inverse. NULL if first call of cacheSolve
     if(!is.null(inv)) {                   ## Checking if mean already cached
          message("Getting Cached Inverse")## Prints message
          return(inv)                      ## Prints the inverse and ends the function
     }
     ## This part of the code is ignored if inverse was cached, but if it was NULL, we will solve for inverse.
     data <- x$get()                       ## Assigns the input matrix to 'data' through the get function
     inv <- solve(data, ...)               ## Find the inverse
     x$setinverse(inv)                     ## Store the inverse for access later
     inv                                   ## Returns the inverse
}