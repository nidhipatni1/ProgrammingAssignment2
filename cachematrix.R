## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = inv, getinv = inv)
}


## Write a short comment describing this function

## This function will : 
## 1.set the matrix
## 2.get the matrix
## 3.set the inverse of the matrix
## 4.get the inverse of the matrix

## This function would help to get the inverse of a matrix. First it will check if the inverse already exists. Is it does, it will use the inverse
## as stored in the cache. Else it will compute the inverse.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

