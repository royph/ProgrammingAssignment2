## This pair of functions can be used to cache calculation
## of the inverse of a matrix.

## This function creates an object from a matrix.
## The object contains a list of functions:
##    set - sets the value of the matrix
##    get - gets the value of the matrix
##    setinverse - sets the value of the inverse of the matrix
##    getinverse - gets the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get, 
           setmean = setmean, 
           getmean = getmean)
  
}


## This function checks an object created by makeCacheMatrix
## for a previously calculated inverse matrix. If no inverse
## matrix has been set, this function solves for the inverse
## of the matrix set in makeCacheMatrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
