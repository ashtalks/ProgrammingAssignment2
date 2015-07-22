## The following is a  pair of functions that cache the inverse of a matrix
## 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(mx = matrix()) {
  m <- NULL
  set <- function(y) {
    mx <<- y
    m <<- NULL
  }
  get <- function() mx
  setminverse <- function(m_inverse) m <<- m_inverse
  getminverse <- function() m
  list(set = set, get = get,
       setminverse = setminverse,
       getminverse = getminverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(mx, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- mx$getminverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- mx$get()
  m <- solve(data, ...)
  mx$setminverse(m)
  m
  
}
