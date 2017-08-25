## The following functions are used to cache the inverse of a matrix
## by caching the results, it will help by save computational process and time

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# The following function returns the inverse of a matrix. If the inverse of a matrix has already been
# calculated, it will return the result without computing it. Otherwise, the function will proceed to 
# calculate the result

cacheSolve <- function(x, ...) {
	 inv <- x$getinverse()
 if(!is.null(inv)){
   message()
   return(inv)
 }
 data <- x$get()
 inv <- solve(data,...)
 x$setinverse(inv)
 inv
}