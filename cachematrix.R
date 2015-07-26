## The function makeCacheMatrix returns a list of 4 functions that can be called
## on the matrix given to is as input. These functions set the matrix, get the
## matrix, set the inverse matrix, and get the inverse matrix.

## For instance, if we let b <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2)),
## we can call the different functions b$get() to return the matrix. If we call
## the funtion b$getInverse() before using the cacheSolve function to actually
## calculate the inverse, we will get a NULL value because it has not yet been
## calculated.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Now, using the cacheSolve function we can call functions from within 
## makeCacheMatrix to actually calculate (and cache) the inverse matrix if it has not 
## already been done so. i.e. cacheSolve(b)
## If we call b$getInverse() now, the inverse of the matrix will be cached.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data) #solve gives the inverse of a matrix
  x$setInverse(i)
  i
}

#b <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
#b$get()
#b$getInverse()
#cacheSolve(b)
#b$getInverse()


