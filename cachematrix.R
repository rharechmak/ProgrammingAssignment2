################################################################################
### Function is designed with the ability to cache the inverse of a matrix
### Input: a matrix
### Output: Object/List
### Object: A collection of local functions
################################################################################
makeCacheMatrix <- function(x = matrix()) {
  ### Initialize the inverse to be null
  i <- NULL
  ### Create list of functions to set and get inverse of matrix values
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  ### Return the list of functions
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

################################################################################
###Function is designed to calculate and cache the inverse of a matrix
### Input:Object/List
### Output: A matrix
################################################################################
cacheSolve <- function(x, ...) {
  ### Initialize i to the current calculated inverse
  i <- x$getinverse()
  ### If the inverse is already calculated return the result
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ### The inverse has not been calculated. Calculate and save the results of the inverse
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
