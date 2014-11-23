## makeCacheMatrix creates a special matrix object, and then cacheSolve will calculates
## the inverse of the matrix.  If the inverse of the matrix has already been calculated,
## it will find it in the cache and returns it.  If not, it will calculate the inverse
## again.
##
## ===============================================================================
## Function Name:  makeCacheMatrix
## Description:    function to creates a special "matrix" object that can cache its
##                 inverse. The object does not calculate the inverse but saves the
##                 matrix to variable x and its inverse to variable m.
##
##   The return object contains the following methods:
##     set:        set the value of the matrix
##     get:        returns the matrix
##     setmatrix:  saves the solved value
##     getmatrix:  returns the cached inverse value
##
## ===============================================================================

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
          x <<- y
          m <<- NULL
      }
 
      get <- function() x
      setmatrix <- function(solve) m <<- solve
      getmatrix <- function() m
      list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## ===============================================================================
## Function Name:  cacheSolve
## Description:    function to obtain the inverse of the matrix object created by 
##                 makeCacheMatrix.  It takes the matrix object as an argument, 
##                 checks whether the inverse value already existed in the cache,
##                 and returns the cached value if it is.  If not, the function
##                 will calculate the inverse of the matrix, saves it into 'x'
##                 cache using the 'setmatrix' method.
##
## ===============================================================================

cacheSolve <- function(x, ...) {
      m <- x$getmatrix()
      if(!is.null(m)) {
          message("getting cached data")
          return(m)
      }

      matrix <- x$get()
      m <- solve(matrix, ...)
      x$setmatrix(m)
      m
}
