## Put comments here that give an overall description of what your
## functions do

##This function, makeCacheMatrix creates a special matrix, which is a list containing 4 functions:
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of the inverse
##4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
       inv <- NULL
       set <- function(y) {
             x <<- matrix(y)
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get,setinverse=setinverse, getinverse=getinverse )
}


## This function calculate the inverse of the matrix created with the above function. if first checks 
## if the inverse is already calculated it skips the calculation and gets the inverse from the cache. 

cacheSolve <- function(x, ...) {
       inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
      
        ## Return a matrix that is the inverse of 'x'
}
