##  Quyen Dong
##  8 MAR 2019
##  CRSE2_WK3_PROG_ASGNMT

##---------------------------------------------------------------------------------------------------
## Second programming assignment will require you to write an R function is able to cache potentially 
## time-consuming computations. For example, taking the mean of a numeric vector is typically a fast operation. 
## However, for a very long vector, it may take too long to compute the mean, especially if it has to be 
## computed repeatedly (e.g. in a loop). If the contents of a vector are not changing, it may make sense 
## to cache the value of the mean so that when we need it again, it can be looked up in the cache rather 
## than recomputed. In this Programming Assignment will take advantage of the scoping rules of the R language 
## and how they can be manipulated to preserve state inside of an R object.

## Write the following functions:
## makeCacheMatrix: 
##    This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: 
##    This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##    If the inverse has already been calculated (and the matrix has not changed), 
##    then the cachesolve should retrieve the inverse from the cache.
## Computing the inverse of a square matrix can be done with the solve function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.
## For this assignment, assume that the matrix supplied is always invertible.
##---------------------------------------------------------------------------------------------------

## makeCacheMatrix: 
##    This function creates a special "matrix" object that can cache its inverse.
##    <<- operator which can be used to assign a value to an object in an environment 
##    that is different from the current environment
makeCacheMatrix <- function(x = matrix()) {
          m <- NULL
          setMatrix <- function(y) {
            x <<- y
            m <<- NULL
          }
          getMatrix <- function() x
          setInverse <- function(inverse) m <<- inverse
          getInverse <- function() m
          list(setMatrix = setMatrix, getMatrix = getMatrix,
               setInverse = setInverse,
               getInverse = getInverse)
}

## cacheSolve: 
##    This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##    If the inverse has already been calculated (and the matrix has not changed), 
##    then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
          message("Getting cached data.")
          return(m)
        }
        data <- x$getMatrix()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
