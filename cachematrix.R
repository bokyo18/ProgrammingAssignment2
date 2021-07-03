## Put comments here that give an overall description of what your
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
## functions do

## Write a short comment describing this function
##creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  b <- NULL
  set <- function(y){
    x <<- y
    b <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) b <<- inverse
  getInverse <- function() b 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Write a short comment describing this function
##That means the environment within which they were defined
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  b <- x$getInverse()
  if(!is.null(b)){
    message("getting cached data")
    return(b)
  }
  mat <- x$get()
  b <- solve(mat,...)
  x$setInverse(b)
  b
}
