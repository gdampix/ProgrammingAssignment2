## Matrix inversion is usually a costly computation and
## there may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly.
##
## following functions acomplish this goal by caching matrix and its inverse 
## in environment for future use

## makecachematrix() function creates a matrix and 
##   returns a list of functions to act on that matrix
##                        (note: matrix must be identity matrix)
##     1. set the value of the matrix
##     2. get the value of the matrix
##     3. set the value of the inverse of matrix
##     4. get the value of the inverse of matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set  <- function(mat){
    x <<- mat
    inv <<- NULL
  }
  
  get <- function(){x}
  
  setinv <- function(inv){inv<<-inv}
  
  getinv <- function(){inv}
  
  return( list(set=set, get=get, setinv=setinv, getinv=getinv) )
}


## cacheSolve() function calculates the inverse of the special "matrix" 
##    created with the makeCacheMatrix() [above] function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse
##    in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached matrix")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  return(inv)
}

