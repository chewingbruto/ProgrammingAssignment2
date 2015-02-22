## Do the inverse of a matrix and if matrix isn't changed
## then cache this inversion

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  msolve <- NULL
  set <- function(y) {
    x <<- y
    msolve <<- NULL
  }
  get <- function() x
  setsolve <- function(s) msolve <<- s
  getsolve <- function() msolve
  
  list(set = set,
       get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix function.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  msolve <- x$getsolve()
  if (!is.null(msolve)) {
    message("return cached data")
    return(msolve)
  }
  mtx <- x$get()
  msolve <- solve(mtx, ...)
  x$setsolve(msolve)
  msolve
}
