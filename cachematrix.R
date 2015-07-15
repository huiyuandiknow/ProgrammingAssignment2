## Date: 07/14/2015
##
## This R file contains 2 R functions for Assignment 2 
## for "Introduction to R" course on Coursera


## makeCacheMatrix creates a "matrix" object that can cache 
## its inverse. This function creates an matrix object with 4 
## functions: set, get, setinverse, and getinverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv

  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If it's been calculated
## earlier, it'll return the cached matrix. 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}


## codes for testing 
# w<- matrix(c(2,1,7,4,5,4,3,4,5),nrow=3,ncol=3)
# solve(w)

# k<- makeCacheMatrix(w)
# sum(cacheSolve(k) == solve(w)) == nrow(w)*ncol(w)



