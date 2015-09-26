## Taking as a start-point the example provided in the assigment, these
## functions are meant to store and review a set of two matrices: an original 
## squared invertible matrix and its inverse. 
## The set of functions also allows to calculate the inverse matrix in case that 
## a new matrix (meant as "original") is cached.

## Emulating the characteristics of the list in the makeVector example, 
## this function aims to create a list of functions that allows to review 
## of define the original matrix (x) and its inverse.

makeCacheMatrix <- function(x = matrix()) {

    im <- matrix(NA, nrow(x), ncol(x))
    ## Initializes an empty matrix with the same dimmensions a x.
    
    setm <- function(y) {
        x <<- y
        im <<- matrix(NA, nrow(y), ncol(y))
    }
    ## Allows the matrix to be redifined by $setm, resets the inverse matriz 
    ## (im) to NA with the new matrix's dimmensions.
    
    getm <- function() x
    ## Defines a function that shows the current (original) matrix.
    
    setinverse <- function(inversem) im <<- inversem
    ## Defines a function that allows to update the matrix to a newly
    ## calculated inverse. 
    
    getinverse <- function() im
    ## Defines a function that shows the current (inverse) matrix.
    
    list(setm = setm, getm = getm, setinverse = setinverse, getinverse = getinverse)
    ## Defines the lists of functions to operate the original and inverse matrices.
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    im <- x$getinverse()
    ## Retrieves the current value of the inverse matrix to
    ## evaluate the existance of NAs.
    
    if(!any(is.na(im))) {
        message("Getting cached inverse matrix")
        return(im)
    }
    ## Evaluates the presence of NAs, if none are found, the 
    ## inverse is printed and the function ends.
    
    im <- solve(x$getm())
    ## If the inverse evaluated before has any NAs, the 
    ## value of the inverse is calculated and stored in im.
    
    x$setinverse(im)
    ## The new value of the inverse matrix is cached.
    
    im
    ## The new inverse matrix is printed.
    
}
