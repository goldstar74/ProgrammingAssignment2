## makeCacheMatrix function below creates a cache of a matrix that makes it easy for computation

## In the makeCacheMatrix, a matrix is specified and it is cached.

makeCacheMatrix <- function(x = matrix()) {
	minv <- NULL
  	set <- function(y) {
    		x <<- y
    		minv <<- NULL
	}
  get <- function() x
  setinverse <- function(inverse) minv <<- inverse
  getinverse <- function() minv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve creates a function that inverses the matrix that we assign and returnes "getting cached inverse matrix" when the inverse matrix is displayed from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        minv <- x$getinverse()
  if(!is.null(minv)) {
    message("getting cached inverse matrix")
    return(minv)
  }
  data <- x$get()
  minv <- solve(data, ...)
  x$setinverse(minv)
  return(minv)
}
