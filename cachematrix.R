##  Matrix inversion is usually a costly computation and there may be some benefit
##  to caching the inverse of a matrix rather than compute it repeatedly

##  makeCacheMatrix function creates a matrix and save it in the Cache
##  It can be retrieved from Cache when required 

makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a matrix and save it
  ## in Cache for future retrieval
  mCache <- NULL
  set <- function(y) {
    x <<- y
    mCache <<- NULL
  }
  get <- function() x
  setminverse <- function(inverse) mCache <<-inverse
  getminverse <- function() mCache
  list(set = set, get = get, setminverse = setminverse, getminverse = getminverse)
}



##  CacheSolve function returns a matrix that is the inverse of the matrix passed
##  It computes the inverse of the special "matrix" returned by makeCacheMatrix 
##  It first checks if the inverse has already been calculated
##  If so, the function should retrieve the inverse from the cache
##  If it is not in the Cache, the function will compute the inverse
##  It assumes the matrix passed is always an reversible matrix.

cacheSolve <- function(x, ...) {
  ## This function returns a matrix that is 
  ## the inverse of 'x'
 
  mInverse <- x$getminverse()
  if(!is.null(mInverse)) {
    message("getting cached data")
    return(mInverse)
  }
  data <- x$get()
  mInverse <- solve(data)
  x$setminverse(mInverse)
  mInverse
  
}
