## These functions are used to enable caching the inverse of a matrix
## Caching a matrix is a potentially time consuming operation and these
##    functions should make a program run faster.

# Example of how to use these two functions:
# -----------------------------------------
# 1) start with a matrix you want to invert (or create a normal matrix)
#      m <- matrix(C(3,7,0,4), nrow=2, ncol=2)      
# 2) use makeCacheMatrix(m) to create a 'special matrix' that caches the inverse of m
#        note: makeCacheMatrix() does not return a matrix; instead it returns
#              a list of 4 functions.
#      cm <- makeCacheMatrix(m)
# 3) use cacheSolve(cm) to return the inverse of the original matrix m
#        note: cacheSolve() will not work on a normal matrix; it will only work on
#              a matrix that was created using makeCacheMatrix()
#      inverse_m <- cacheSolve(cm)


## makeCacheMatrix() is used to create a 'special' matrix that uses caching

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL

   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x

   setinverse <- function(inverse_matrix) m <<- inverse_matrix
   getinverse <- function() m

   # returns a list containing 4 functions: set, get, setinverse, and getinverse
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}



## cacheSolve(x) is used to return the inverse of matrix 'x'.
##    note: x is not an ordinary matrix but rather one created with makeCacheMatrix()

cacheSolve <- function(x, ...) {
   m <- x$getinverse()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }

   data <- x$get()
   m <- solve(data, ...)
   x$setinverse(m)

   # Return a matrix m that is the inverse of 'x'
   m


}
