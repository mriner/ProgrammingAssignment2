## makeCacheMatrix: This function creates list of 4 function
## get, set for the matrix parameter
## getinv and setinv for the inverse matrix
## the function assumes the matrix is invertible and use "solve" to try to invert the matrix
## 
makeCacheMatrix <- function(x = matrix()) {
  message("creating get, set, getinv, setinv")
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inv <<- solve(x)
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}


## this function tries to return the inverse of the paramter matrix passed to the makeCacheMatrix above
## it takes as paramter the output of makeCacheMatrix 
## it returns the inverted matrix from the makeCacheMatrix when available
## or it computes the invert, stores it in the makeCacheMatrix object and return it
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat )
  x$setinv(inv)
  inv
  
  }

