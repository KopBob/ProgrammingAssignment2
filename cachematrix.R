## Assignment: Programming Assignment 2: Lexical Scoping

## This function creates a special "matrix"
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) inv
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

## Small test
# m <- makeCacheMatrix()
# m$set(matrix(c(1:4), nrow = 2))
# m_inv <- cacheSolve(m)
# stopifnot(all(m_inv == solve(m$get())))
# stopifnot(all(m$getinv() == solve(m$get())))
