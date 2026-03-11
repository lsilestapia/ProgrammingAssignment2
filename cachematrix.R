## Put comments here that give an overall description of what your
## functions do
# The exercise demonstrates closures + caching in R

## Write a short comment describing this function
# builds the infrastructure (storage + methods).
makeCacheMatrix <- function(x = matrix(numeric(0), nrow = 0, ncol = 0)) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
#logical part, it calculate if needed, otherwise reuse cache.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
