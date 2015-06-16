# Author: Milen Angelov
# Programming Assignment 2
# Course ID: rprog-015

## Description: 
## This script contains a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
## It takes a matrix and returns a list of setter and getter functions to be
## performed over the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    matData <- NULL; # creates an empty matrix
    
    set <- function( newVal ){ # set the value to the matrix
        x <<- newVal; # set the new value
        matData <<- NULL; # clear the old one
    }
    
    get <- function() x; # get the value out of the matrix
    
    # Function for setting the inverse value
    setInverseMatrix <- function( invVal ) matData <<- invVal;
    
    # Function for getting the inverse value
    getInverseMatrix <- function() matData;
    
    # List of the functions defined above
    list(set = set, get = get, 
         setInverseMatrix = setInverseMatrix, 
         getInverseMatrix = getInverseMatrix);
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.
##
## It takes as parameter a metrix created with makeCacheMatrix functions
## and returns its inverse
cacheSolve <- function(x, ...) {
    
    # first let's get the cached value for the inverse
    invVal <- x$getInverseMatrix();
    
    # check if the cache is empty
    if( !is.null( invVal ) ){
        # it is not empty so return it
        message("Returning cached value");
        return( invVal );
    }
    
    # so the cache is empty. Let's create it then
    val <- x$get();
    # now calculate the inverse and assign it to a new matrix
    invVal <- solve( val );
    # cache it for future use
    x$setInverseMatrix( invVal );
    # return its inverse
    return( invVal );
}
