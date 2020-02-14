
## This function creates a special "matrix" object that can cache its inverse.
# This function is a list containing a function to :
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse matrix
# get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  InvMatrix <- NULL
  SetMatrix <- function(y) {
    x <<- y
    InvMatrix <<- NULL
  }
  GetMatrix <- function() x
  SetInverse <- function(inverse) InvMatrix <<- inverse
  GetInverse <- function() InvMatrix
  list(SetMatrix = SetMatrix,      # set the value of matrix
       GetMatrix = GetMatrix,      # get the value of matrix
       SetInverse = SetInverse,    # set the inverse of matrix
       GetInverse = GetInverse)    # get the inverse of matrix
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  InvMatrix <- x$GetInverse()
  if(!is.null(InvMatrix)) {
    message("Getting cached inversed matrix")
    return(InvMatrix)
  }
  data <- x$GetMatrix()
  InvMatrix <- solve(data, ...)
  x$SetInverse(InvMatrix)
  InvMatrix
}
