## Put comments here that give an overall description of what your
## functions do


## makeCacheMatrix
##  Stores a matrix (x) and its inverse (inv), if calculated.
##  Returns a list of functions:
##   set(y): stores a new matrix (y), and clears the inverse matrix (inv) in the cache;
##   get(): returns the matrix (x) stored;
##   set_inv(new_inv): stores a new inverse (new_inv) of the matrix stored (x);
##   get_inv(): returns the inverse matrix (inv) in the cache, if it has been 
##              calculated already 
##   Obs 1: When using the set_inv function, make sure that the argument being
##          passed is, in fact, the inverse. I advise to only call this function
##          inside the cacheSolve function.
##   Obs 2: Also, the get_inv function will only return the inverse if it has been 
##          calculated already. I advise to only call this function inside the
##          cacheSolve function.

## cacheSolve
##  Parameter:
##   x: list returned by function makeCacheMatrix
##  Returns the inverse matrix.
##   If the inverse has already been calculated, it gets the matrix from the cache 
##   Otherwise, it calculates the inverse, and saves the inverse in the cache


## Write a short comment describing this function
## makeCacheMatrix:
##  Stores a matrix (x) and returns a list of functions (set, get, set_inv, get_inv)

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
## cacheSolve: R
##  Returns the inverse matrix of x

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
