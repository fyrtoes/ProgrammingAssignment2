## Put comments here that give an overall description of what your
## functions do

## function makeCacheMatrix initializes an matrix-like object that
##                          stores its inverse value in the cache

## function cacheSolve computes the inverse of the makeCacheMatrix object
##                          using it's methods

## Write a short comment describing this function

###############################################################################
#
# makeCacheMatrix - an object that takes in arguments like a matrix.
#                   It contains the following variables and methods.
# inv - Free variable. Stores the inverse of the matrix passed as an
#            attribute of this object.
# set() - function used as an alternative method to initialize this object.
# get() - function used to return the content set to this object.
# setinverse() - function used by function 'cacheSolve' to store
#                         the inverse of the matrix unto 'inv'.
# getinverse() - function used by function 'cacheSolve' to return
#                         the value in 'inv'.
# list() - Last line of function makeCacheMatrix. Returns the 4 functions
#                         listed above.
# 
###############################################################################

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL             ## Always reinitializes 'inv' when a new 
                          ##        Cache Matrix is created
  set <- function(y) {    ## Constructor function.
                          ##        2nd way of initializing a 
                          ##        makeCacheMatrix object.
    x <<- y
    inv <<- NULL          ## Similar to line #7
  }
  get <- function() x     ## Method used to getting the contents of a
                          ##        makeCacheMatrix object. Used in cacheSolve
  setinverse <- function(inverse) inv <<- inverse  ## Used by cacheSolve
  getinverse <- function() inv      ## Used by cacheSolve
  list(set = set, get = get,        ## List of functions returned by
       setinverse = setinverse,     ##       makeCacheMatrix object.
       getinverse = getinverse)
}


## Write a short comment describing this function

###############################################################################
#
# cacheSolve - takes in a makeCacheMatrix object and returns its inverse.
#
###############################################################################

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()            ## Acknowledges 'inv' from the object passed.
  if(!is.null(inv)) {              ## if 'inv' is null, skip the if-block
    message("getting cached data") ## if 'inv' is NOT null, return it.
    return(inv)                    ## return 'inv'
  }
  data <- x$get()                  ## Assign to 'data' the contents of matrix
  inv <- solve(data, ...)          ## Assign to 'inv' the inverse of 'data'
  x$setinverse(inv)                ## Use setinverse() to assign 'inv' to
                                   ##     the 'inv' attribute
                                   ##     in the makeCacheMatrix object
  inv                              ## Returns 'inv'
}
