## Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation. The pair of functions in 
## this R source file caches the inverse of a matrix so that getting it 
## repeatedly only involves computing it when the matrix has been reset.

## Use example :

## > A <- makeCacheMatrix(matrix(c(1:4),2,2)) # creating and setting
## > cacheSolve(A) # getting A's inverse
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## Now let's check whether the result looks like A's inverse without
## bothering about computing it again (the cache will be used instead) :

## > cacheSolve(A) # a message should be issued about the cache this time
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## > cacheSolve(A) %*% A$get() # getting it again and using it
## getting cached data
##      [,1]         [,2]
## [1,]    1 1.776357e-15
## [2,]    0 1.000000e+00

## Finally let's check whether resetting A also changes its inverse

## > A$set(matrix(c(2:5),2,2))
## > cacheSolve(A)
##      [,1] [,2]
## [1,] -2.5    2
## [2,]  1.5   -1


## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverseToPush) i <<- inverseToPush
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not been reset), then the inverse from the
## cache is retrieved. As a side effect, a message is issued about the
## the cache being used. Otherwise the inverse is calculated from scratch.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
