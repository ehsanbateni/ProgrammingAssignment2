## This function creates a matrix object for matrix x which is actually  a list of functions trying to set and get the value of matrix and its inverse. 
## It also caches the value of matrix inverse in variable 'inv'(leveraging lexical scoping) which is local to make CacheMatrix() but a free variable inside the internal functions. 

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


## This function first checks if the inverse of matrix in the object x is alredy calculated or not. If yes it gets the cached inverse from the object$getinverse. 
## If no it calculates the inverse and caches it via object$setinverse. Obviously if the matrix itsef has been modified via object$set the cached inverse has been nulled again
## and is calculated when cacheSolve() invoked again

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
      if(!is.null(inv)) {
      	message("getting cached data")
            return(inv)
      }
      inv <- solve(x$get(), ...)
      x$setinverse(inv)
      inv
}
        
