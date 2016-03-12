## Author: Irian Hornblower
## Created: March 11, 2016
## Sources: The following code was written following the example "Caching the Mean of a Vector" provided by Coursera instructors

## The function makeCacheMatrix creates a special "matrix" object that can cache the inverse of the matrix it receives as an argument
## MakeCacheMatrix creates a special "matrix" that is a list containing four(4) functions to
## 1- set the value of the matrix 2- get the value of the matrix 3- set the value of the inverse of the matrix
## 4- get the value of the inverse of the matrix

## Note: The function solve() is used in setinverse() to get the inverse of a matrix and assuming that the given matrix is invertible


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setmatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(solve)  m <<- solve
  getinverse <- function() m
  
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
  
  }

## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieves
## the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m) 
  }
  data <- x$getmatrix()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
