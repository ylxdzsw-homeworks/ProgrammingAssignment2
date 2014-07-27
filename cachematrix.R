## This script creats a special matirx that can store it's inverse
 
## creats the matrix
makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) I <<- inverse
  getinverse <- function() I
  list(set = set, get=get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x', but it is not sure that
## Whether a real caculation is done
cacheSolve <- function(x, ...) {
  I <- x$getinverse()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setinverse(I)
  I
}
