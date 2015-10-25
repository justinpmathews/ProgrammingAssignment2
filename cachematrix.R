## The makeCacheMatrix and cacheSolve functions faciliate getting the inverse of a matrix while also saving a 
## cache of the inverse in order to save time should the inverse be needed again.


## The makeCacheMatrix function creates a vector - in the form of a list of functions - 
## that facilitates storing and retrieving the value of a matrix, and storing and retrieving a cached inverse of that matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #sets default 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve function checks to see if a cached inverse for a matrix is already stored through the makeCacheMatrix function.
## If not, cacheSolve calculates the inverse of the matrix, stores a cache of the result in the vector created by makeCacheMatrix,
## and returns the inverse of the matrix to the console. 

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}