##The first function 'makeCacheMatrix' takes a matrix as argument and returns a list consisting four functions, in other words, 
##the first funciton creates a new data type(let's name it as 'makeMatrix') that is a list and each element of the list is a funciton. The second function
##'cacheSolve' takes a 'makeMatrix' as argument  and returns the inverse of the matrix taken by the first funciton if the inverse has not been cached.
##'##Otherwise, the second funciton will print "getting cached data" and return the inverse afterwards.

## Thid funciton creates a new 'matrix' object that is capable of caching its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setInv <- function(inverse) inv <<- inverse
  
  getInv <- function() inv
  
  list(set = set, get = get, setInv = setInv, getInv = getInv)

}

## This function computes the inverse of the new 'matrix' object created by 
## makeCacheMatrix above. If the inverse has already been calculated (which meansh the 
## matrix has not changed), then it will return the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
