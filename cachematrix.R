## Firs I will create a function makeCacheMatrix that will
## create a special "matrix" object that can cahe its inverse.
## Second I create a function cacheSolve wich will compute the
## inverse of the special "matrix" returned by makeChaceMatrix
## function. 

## This first function stores 4 functions set, get, setInvers
## and getInverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This function computes the inverse of the matrix returned
## by the makeCacheMatrix above
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
  messsage("getting cached data")
  return(i)
    }
  data <- x$get()
  i <- solve(data,...)
  x$setInverse(i)
  i
}
        ## Return a matrix that is the inverse of 'x'
