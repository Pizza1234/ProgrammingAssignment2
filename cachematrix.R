##source('F:/Coursera/R Programming/wd/cachematrix.R')
##a<-matrix(3:6,2,2)
##a
####[,1] [,2]
####[1,]    3    5
####[2,]    4    6
##cacheSolve(makeCacheMatrix(a))
####[,1] [,2]
####[1,]   -3  2.5
####[2,]    2 -1.5
## makeCacheMatrix and cacheSolve are two functions that together
## will cache the inverse of the matrix that is given as an argument
## to the function when called.
## makeCacheMatrix defines the four functions required to set and get both the
## original matrices and their inverses. Finally, it returns the list object
## so that the functions are accessible for later use.
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  ## put the matrix into cache memory
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  ## retrieve the matrix from cache memory
  get <- function() x
  ## set the inverse matrix into cache
  setsolve <- function(solve) s <<- solve
  ## retrieve the inverse matrix from cache
  getsolve <- function() s
  ## return the list object with four matrices
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
## cacheSolve first checks if there exists an inverse to the
## matrix provided as an argument to the function and, if not,
## calculates and caches that inverse for later retrieval
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  ## check if there is an inverse matrix already
  ## if yes, return it
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  ## otherwise, calculate a inverse from the source matrix
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
