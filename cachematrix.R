## The following pair of functions cache or eventually compute the inverse of a matrix
## 
## The first function "makeCacheMatrix" creates a special "matrix" object that is able to cache its inverse.


makeCacheMatrix <- function(x = matrix()) {  ## In these two first lines of the function the matrix x and its inverse mat are just initialised.
  mat <- NULL
  set <- function(y){       ##This set function here creates an environment that makes it possible to access and initialise x and mat which are in the
    x <<- y                 ## in the parent environment (in this case the makeCacheMatrix environment) hence the sign '<<' so that x can be updated to y and mat can be
    mat <<- NULL            ##emptied
  }
  get <- function() x    ## get returns the 'matrix' x stored in the main function.
  setinverse <- function(inverse) mat <<- inverse
  getinverse <- function() mat
  ## setinverse and getinverse store the value of the input in a matrix 'mat' into the main function makeCacheMatrix (setinverse) 
  ## and return it (getinverse).
  ## And the 'mat', input of setinverse, is supposed to be the inverse of the matrix 'x'. 
  ## Since mat is defined in the parent environment we use '<<' once again to access it.
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
} ##In the above line, the naming list is used so that to allow the use of $ to access the functions by theri names.

## The CacheSolve function below computes the inverse of the special matrix returned by makeCacheMatrix function above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve that inverse from
## the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x', the first thing cachSolve does is to verify the value 'mat', stored previously 
  ## with getinvers, if it exists and is not NULL. If it exists in memory, it simply returns the value 'mat'.
  
  
  mat <- x$getinverse     ## makeCacheMatrix is a function that stores a list of functions; to use these functions stored in it, 
                          ## one needs to subset the main function assigned to x beforehand.
  if(!is.null(mat)) {
    message ("getting cache data")
    return(mat)
  }
  data <- x$get()
  mat <- solve(data,...)
  x$setinverse(mat)
  mat
}
