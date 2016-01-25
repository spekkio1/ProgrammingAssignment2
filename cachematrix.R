## Together, these functions allow the user to create a sort of matrix "object"
## and compute its matrix inverse.  If its inverse has already been computed 
## then the cacheSolve function does not need to recompute it --- 
## it will obtain it from the cached value, saving computer resources and time.

## the makeCacheMatrix function takes a matrix as its only argument
## and returns a list of functions which allow you to re-set the matrix data, 
## get the matrix data, set the inverse (via caching), or get the inverse. 
##  Whatever matrix is passed to it is automatically stored 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function takes as its first argument an object created by the 
## makeCacheMatrix function.  
## It tries to get the cached inverse.  If the cached inverse is already there, 
## then it simply returns it.  
## If the inverse has not been computed yet, then it is computed, and the 
## setinv() function of makeCacheMatrix() is used to cache it.  
## Lastly the inverse is returned.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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

