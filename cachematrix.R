## Code for the R course R Programming Week 3
## The functions are allow us to generate a special type of matrix that has the capability to store its inverse

## Creates a special object that allows to store a cache a version of its inverse
## that is useful to simplify calculations where the inverse might be needed over and over again

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This functions calculates the inverse of a matrix for the special matrix type "makeCacheMatrix"
## It will try to use a cached result before attemping to re-calculate the inverse
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached inverse matrix")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  ## Return a matrix that is the inverse of 'x'
}

# Test 1: Generate square matrix
x <- matrix(1:4, 2, 2)
m <- makeCacheMatrix(x)
# Check that the matrix is stored correctly
m$get()
# Solve once using solver
cacheSolve(m)
# Retrieve once from cache
cacheSolve(m)