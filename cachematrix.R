## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: returns a list of functions (set, get, set_inv, get_inv)
## cacheSolve: returns the inverse function


## Write a short comment describing this function

## makeCacheMatrix
##  Returns a list of functions.
##   set(y): stores a new matrix (y), clears the inverse matrix in the cache;
##   get(): returns the matrix stored;
##   set_inv(new_inv): stores a new inverse to the matrix stored;
##   get_inv(): returns the inverse matrix in the cache, if it has been calculated already 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set_inv <- function(new_inv) inv <<- new_inv
    get_inv <- function() inv
    list(set=set, get=get, set_inv=set_inv, get_inv=get_inv)
}



## Write a short comment describing this function

## cacheSolve
##  Returns the inverse matrix.
##   If the inverse has already been calculated, it gets the matrix from the cache 
##   Otherwise, it calculates the inverse, and saves the inverse in the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$get_inv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$set_inv(m)
    m
}
