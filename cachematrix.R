## This functions are able to cache the calculation of the inverse of a given matrix 'x'.
## If the contents of a then matrix 'x' vector are not changing, and the inverse of 'x' 
## is calculated and cached, it will not be recalculated, but looked up in the cache.
## This functions will make use of scoping rules and global assignment with <<-.

makeCacheMatrix  <- function(x = matrix()) {
  ## Creates a special "vector", which is really a list containing a function to:
  ## set the value of the matrix 'x'
  ## get the value of the matrix 'x'
  ## set the value of the inverse of the matrix 'x'
  ## get the value of the inverse of the matrix 'x'
    m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Get the cached value of the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    ## Inverse of 'x' is already cached, was previously calculated
    message("getting cached data")
    return(m)
  }
  ## Inverse of 'x' is not cached, has to be calculated and cached
  ## Get matrix 'x'
  data <- x$get()
  ## Calculate inverse of matrix 'x'
  m <- solve(data, ...)
  ## Set and cache the value of the inverse of matrix 'x'
  x$setinverse(m)
  ## Return calculated inverse of matrix 'x'
  m
}

