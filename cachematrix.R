## The aim of given functions is to output inverse of a given matrix such that 
## a) function makeCacheMatrix stores given matrix and its inverse in cache
## b) function cacheSolve computes the inverse either from cache or directly



## Function makeCacheMatrix take any (regular) matrix and outputs a 
##list of four functions: 
## 1) setting the matrix in cache, 
## 2) getting the matrix from cache
## 3) setting the inverse of a matrix in cache
## 4) getting the inverse of a matrix from cache 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function cacheSolve computes n inverse of an entry matrix,
## either from cache if it is there
## or by computation otherwise

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("Getting cached matrix...")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
