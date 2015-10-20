## This function creates a special "matrix" object that can cache its inverse.
##Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly

makeCacheMatrix <- function(x = matrix()) {
  in <- NULL
  set <- function(y) {
    x <<- y
    in <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) in <<- inverse
  getinverse <- function() in
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  in <- x$getinverse()
  if(!is.null(in)) {
    message("getting cached data")
    return(in)
  }
  data <- x$get()
  in <- solve(data, ...)
  x$setinverse(in)
  in
}
