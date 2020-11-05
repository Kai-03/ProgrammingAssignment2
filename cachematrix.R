## Programming Assignment2 - R Programming
## Author: Kai-03
## Date: 2020-11-5
## -----------------------------------------------------------------------------
## Description:
##  makeCacheMatrix($matrix)
##  -This function creates a special "matrix" object that can cache its inverse.
##  -Creates and returns a list of functions: [set(), get(), setinv(), getinv()]
##
##  cacheSolve($specialmatrix)
##  -This function computes the inverse of the special "matrix" returned by 
##   makeCacheMatrix above, either from cache or a new calculation.
##  -Returns a matrix inverse.
## -----------------------------------------------------------------------------

# Function: Create functions list to set/get [matrix,inverse].
# Return: [set(), get(), setinv(), getinv()]
makeCacheMatrix <- function(x = matrix()) {
  # Assign value
  i <- NULL
  # Set value of matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  # Get value of matrix
  get <- function() x
  
  # Set value of inverse
  setinv <- function(inv) i <<- inv
  # Get value of inverse
  getinv <- function() i
  # Make list
  list(set=set, get=get,setinv=setinv,getinv=getinv)
}

# Function: Retrieve Cache or Solve for new Inverse matrix.
# Return Inverse matrix either from existing cache or new solve().
cacheSolve <- function(x, ...) {
  i <- x$getinv()
  # Check if Cache exists - Proceed to Cache:
  if(!is.null(i)){
    message("Retrieving existing cache..")
    return(i)
  }
  # Get Basic data - Solve data - Set inverse:
  data <- x$get()
  i <- solve(data, ...)
  print("Solving inverse..")
  x$setinv(i)
  
  ## Return a matrix that is the inverse of 'x'
  i
}
