## These functions are used to enable caching the inverse of a matrix
## Caching a matrix is a potentially time consuming operation and these
##    functions should make a program run faster.

# Documentation of how to use these two functions:
# ------------------------------------------
# 1) start with a matrix you want to invert (or create a new matrix)
#      m <- matrix(C(3,7,0,4), nrow=2, ncol=2)
#      
# 2) use makeCacheMatrix() to create a 'special object' that stores both a matrix and its inverse
#        makeCacheMatrix() does not calculate anything
#        makeCacheMatrix() returns a list of 4 functions:
#              set(mtx)       # to store a new matrix
#              get()          # to return the stored matrix
#              setinverse()   # to store an inverse matrix
#              getinverse()   # to return an inverse matrix
#  
# 3) use cacheSolve(cm) to return the inverse of the matrix stored in cm
#       note: you cannot pass a normal matrix to cacheSolve()
#             cacheSolve(m) will not work if m=matrix and will give an error message.
#             the parameter p passed to cacheSolve(p) needs to be an object of the type 
#             returned by makeCacheMatrix(); i.e. a list of functions: set, get, setinverse, and getinverse
#      inverse_m <- cacheSolve(cm)
#
# Sample code using these two functions:
# -------------------------------------
#> m <- matrix(c(1,2,3,4),nrow=2,ncol=2)     # create a small matrix to work with
#> sm <- makeCacheMatrix(m)                  # create an object that can store a matrix (m) and its inverse
#> sm$get()                                  # returns matrix m
#> inv1 <- cacheSolve(sm)                    # returns calculated inverse of matrix stored in sm; also caches inverse in sm
#> inv2 <- cacheSolve(sm)                    # returns cached inverse matrix
#> sm$set(matrix(c(2,4,6,8),nrow=2,ncol=2))  # stores new matrix and erases cached inverse matrix in sm
#> sm$get()                                  # returns new matrix stored in sm from the previous command
#> inv3 <- cacheSolve(sm)                    # returns new calculated inverse matrix; also caches inverse in sm



## makeCacheMatrix() is used to create a 'special' matrix that uses caching; it returns a list of 4 function

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL

   set <- function(y) {
      x <<- y
      m <<- NULL              # clear cache
   }
   get <- function() x

   setinverse <- function(inverse_matrix) m <<- inverse_matrix
   getinverse <- function() m

   # return a list containing 4 functions: set, get, setinverse, and getinverse
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}



## cacheSolve(x) does 2 things; 1) it returns an inverted matrix 2) it stores that inverted matrix in x
##    note: x is not a matrix; instead it is a list of functions created with makeCacheMatrix()

cacheSolve <- function(x, ...) {
   m <- x$getinverse()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }

   data <- x$get()                   # data is the matrix stored in x
   m <- solve(data, ...)             # compute the inverse of data and store in m
   x$setinverse(m)                   # save the calculated inverse so it won't have to be calculated again

   # Return a matrix m that is the inverse of 'x'
   m


}
