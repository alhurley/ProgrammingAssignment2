##cache the inverse of a matrix so that when we need it again,
##it can be looked up in the cache rather than recomputed

##The first function, makeCacheMatrix, creates a special "vector", 
##which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the inverse of the matrix
##get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
     calc_inv <- NULL
     set <- function(y) {
          x <<- y
          calc_inv <<- NULL
     }
     get <- function() x
     setinv <- function(solve) calc_inv <<- solve
     getinv <- function() calc_inv
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}



## returns the inverse of matrix x

cacheSolve <- function(x, ...) {
     calc_inv <- x$getinv()
     if(!is.null(calc_inv)) {
          message("getting cached data")
          return(calc_inv)
     }
     data <- x$get()
     calc_inv <- solve(data, ...)
     x$setinv(calc_inv)
     calc_inv
}
