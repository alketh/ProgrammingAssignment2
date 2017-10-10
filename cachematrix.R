# This function creates a special "matrix" object that can cache its inverse.
# The function does return a list of functions to set the values of a matrix
# or retrieve the values of a cached matrix. This also applies to the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function() m <<- solve(x)
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# To save computation time the cached matrix is returned in case it hase already been inverted.
# Otherwise the matrix is inverted.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}

# Usage:
# funs$set(matrix(1:4, 2))
# funs$get()
# funs$setinv()
# funs$getinv()

