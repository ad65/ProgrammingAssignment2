## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
      x <<- y
      m <<- NULL
  }
  get <- function() x
  setInv <- function(inv) m <<-inv
  getInv <- function() m
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## by computing the inverse of the matrix from
  ## makeCacheMatrix() unless the inverse has
  ## been calculated and it retrieves it from the cache.
  m <- x$getInv()
  if ( ! is.null(m)) {
      message("getting cached data")
      return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInv(m)
  m
}
