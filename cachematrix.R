## The following two functions cache the inverse of a matrix.

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse with following operations.
## set the value of the matrix
## get the value of the matrix
## set the inverse of matrix
## get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,get= get,
       setinverse = setinverse, getinverse = getinverse)

}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix function. 
## First it is checking whether the inverse is already cached, if true skip the computation and return the cached inverse
## Otherwise inverse of the matrix is calculated and cached.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)){
      message("getting cached data")
      return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinverse(inv)
     inv
}
