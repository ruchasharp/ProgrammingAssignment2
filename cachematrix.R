## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function sets the matrix for which the inverse
## is to be computed and returns a list of get/set functions
## for the matrix as well as the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  setm <- function(y) {
    x <<- y
    m <<- NULL
  }
  getm <- function() x
  setinvmatrix <- function(invmatrix) m <<- invmatrix
  getinvmatrix <- function() m
  list(setm = setm, getm = getm,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)

}


## Write a short comment describing this function
## this function computes the inverse of the matrix 
## given to the above function, and subsequently
## if that matrix does not change, returns the cached
## value of the inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinvmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getm()
  m <- solve(data)
  x$setinvmatrix(m)
  m
  
  
}
