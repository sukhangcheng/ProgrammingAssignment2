## This is the script for assignment 2 containing a pair of functions that cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()){
  iv <- NULL
  set <- function(y){
    x <<- y
    iv <<- NULL
  }
  get <- function() x
  setiv <- function (inverse) iv <<- inverse
  getiv <- function() iv
  list(set = set, get = get, 
       setiv = setiv,
       getiv = getiv)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, cachesolve should retrieve the inverse from the cache
## Return a matrix that is the inverse of 'x'


cacheSolve <- function(x, ...) {
  iv <- x$getiv()
  if(!is.null(iv)) {
    message("getting cached data")
    return(iv)
  }
  data <- x$get()
  iv <- matrix(data, ...)
  x$setiv(iv)
  iv
}
