## These are a pair of functions that cache the inverse of a matrix


## The makeCacheMatrix creates a special matrix object that can cache its inverse
## It returns a list of functions that can be called to either set or get the matrix
## or its inverse

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


## The cacheSolve function computes the inverse of the special matrix created in
## makeCacheMatrix. The function first checks to see if the inverse has already been cached
## and if it has returns that value. Otherwise it will compute the inverse, cache it, and
## then return it.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  
  i
  
}

