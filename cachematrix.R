## Gets cached inverse or else calls solver and caches results
makeCacheMatrix <- function(x = numeric()) {
   invx <- NULL
   set <- function(y) {
     x <<- y
     invx <<- NULL
   }
   get <- function() x
   setinv <- function(solve) invx <<- solve
   getinv <- function () invx
   list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


## Calculates inverse of square matrix x
## If cached not available

cacheSolve <- function(x, ...) {
  invx <- x$getinv()
  if(!is.null(invx)) {
    message("getting cached data")
    return(invx)
  }
  data <- x$get()
  invx <- solve(data, ...)
  x$setinv(invx)
  invx
}
