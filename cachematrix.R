## makeCacheMatrix sets a matrix.
makeCacheMatrix<- function(x=matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}



## caches/returns the inverse matrix of matrix set by the makeCacheMatrix function if 
## the same matrix is passed to cachesolve it doesn't compute the solution but presents
## the cached inverse and prints "getting cached data"

cachesolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
