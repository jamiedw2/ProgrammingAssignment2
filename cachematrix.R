## There are two functions: makeCacheMatrix() and cacheSolve().
## These create a special matrix and cache its inverse.

## makeCacheMatrix() creates a special matrix. It returns a list
## of functions that sets the matrix, gets the matrix, sets its
## inverse, and gets its inverse.

makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL
  set <- function(y) {
    x <<- y
    matinv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) matinv <<- solve
  getinv <- function() matinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve() checks to see if the matrix inverse has already been
## calculated. If so it returns the inverse, otherwise calculates it
## and adds it to the cache.

cacheSolve <- function(x, ...) {
  matinv <- x$getinv()
  if(!is.null(matinv)) {
    message("getting cached data")
    return(matinv)
  }
  data <- x$get()
  matinv <- solve(data, ...)
  x$setinv(matinv)
  matinv
}
