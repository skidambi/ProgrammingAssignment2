## This file contains two functions - cacheSolve and makeCacheMatrix
## Together these functions make repeated retrieval of the inverse of a matrix more efficient
## Example usage:
##   x = matrix(1:4,2:2)
##   a = makeCacheMatrix(x)
##   b = cacheSolve(a)
## To verify results:
##   a %*% b should return a unit matrix (build into function as next step)

## makeCacheMatrix takes an argument of type matrix and returns a list of 4 functions:
##		set/get -> set/get the value of the matrix object inside the function object
##		setinv/getinv ->set/get the inverse of the matrix object inside the function object

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
	# function to set the matrix object. Reset inv to ensure recalculation
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
	# function to return the stored matrix object
  get <- function() x
	# function to set inv to whatever value is passed in
	# Note: No error checking done here, calling function should do that
  setinv <- function(invMatrix) inv <<- invMatrix
	# function to return the store inv object, NULL or otherwise
  getinv <- function() inv
	# Return a list of above 4 functions
  list(set = set,
			 get = get,
       setinv = setinv,
       getinv = getinv)

}


## cacheSolve takes an argument of type makeCacheMatrix and returns the  inverse of the
## matrix object stored in this function object. If the inverse was never computed before
## it just retrieves that value. Otherwise it computes the inverse of the stored matrix object,
## sets it in the function object and returns the inverse

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'

  if(
      class(x) != "list"
      || names(x) != c("set", "get", "setinv", "getinv")
      || all( sapply(x,class)=="function" ) != TRUE
    ) {
    # argument does not contain the functions we expect to use
    stop("Invalid argument; must be a function object of makeCacheMatrix()
  Aborting...")
    
  }

	# see whether the "inv" object in this function object is NULL, if it isn't, return
  # that value and exit
  invMatrix <- x$getinv()
  if(!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix)
  }

	# "inv" hasn't been computed yet, so get the stored matrix and calculate its inverse
  origMatrix <- x$get()
	# tryCatch to fail gracefully if solve fails
  tryCatch(
    invMatrix <- solve(origMatrix, ...),
    error = function(e) {
			# show a message describing what happened and exit after printing the error we received
      message("cacheSolve failed while solving for the matrix inverse:")
      stop(e)
    }
  )

	# set "inv" in the function object to the computed inverse
	# set the inverse
  x$setinv(invMatrix)
	# return the inverse explicitly in case setinv changes in future to not return it
	invMatrix
}
