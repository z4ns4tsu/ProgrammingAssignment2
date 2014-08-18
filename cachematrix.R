## Programming Assignment 2 for Coursera's R Programming course. This script
## calculates the inverse of a matrix and caches the result to speed up future
## inversions of the same matrix.

## Takes a matrix as input and adds the ability to cache its inverse. Use with
## cachesolve() below.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL

  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  get <- function() x

  setsolve <- function(solve) m <<- solve

  getsolve <- function() m

  list( set = set,
        get = get,
        setsolve = setsolve,
        getsolve = getsolve
        )
}


## Calculates the inverse of a matrix using base::solve() and stores the result
## in a local cache. Checks first to see if the inverse is already cached and
## returns that instead if so.
cacheSolve <- function(x, ...) {
  m <- x$getsolve()

  if(!is.null(m)) {
    message("Getting cached data...")
    return(m)
  }

  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
