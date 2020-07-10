makeCacheMatrix <- function(x = matrix()) {
  invr <- NULL
  set <- function(y) {
    x <<- y
    invr <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invr <<- inverse
  getinverse <- function() invr
  list(set=set, get=get, setinverse=setinverse, getinverse = getinverse)
}
cacheSolve <- function(x, ...) {
  invr <= x$getinverse()
  if(!is.null(invr)) {
    message("Getting Cached Data:-")
    return(invr)
  }
  matrx <- x$get()
  invr <- solve(matrx, ...)
  x$setinverse(invr)
  invr
}

