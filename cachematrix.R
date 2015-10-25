## The following pair of functions cache or eventually compute the inverse of a matrix
## 
## The first function "makeCacheMatrix" creates a special "matrix" object that is able to cache its inverse.
## But, beforehand, the function checks to see if that inverse has already been calculated. If so, it gets
## that inverse and does not perform any computation. If not, it performs the inverse of the matrix and sets it in the cache.
## 
## Actually the function a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        mat <- NULL
        set <- function(y){ ## set is a function that changes the matrix stored in the main function
        x <<- y             ## Substitution of the 'matrix' x with y (the input) in the main function (makeCacheMatrix)
                            ## variable x in the containing environment (global in this case) is updated to be y  
                            
        mat <<- NULL        ## Restoring to null the value of the inverse, because the old inverse of the old matrix is not needed anymore
                            ## the new inverse needs to be recalculated through the function cacheSolve.

        }
        get <- function() x ## get is a function that returns the 'matrix' x stored in the main function. Doesn't require any input
        setinverse <- function(inverse) mat <<- inverse
        getinverse <- function() mat
                           ## setinverse and getinverse are functions very similar to set and get. They don't calculate the inverse, but 
                           ## they simply store the value of the input in a matrix 'mat' into the main function makeCacheMatrix (setinverse) 
                           ## and return it (getinverse).
                           ## And the 'mat', input of setinverse, is supposed to be the inverse of the matrix 'x'. 
                           ## However it simply stores the value of the inverse
                           
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The CacheSolve function below computes the inverse of the special matrix returned by makeCacheMatrix function above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve that inverse from
## the cache.
## 

cacheSolve <- function(x, ...) {
                              ## Return a matrix that is the inverse of 'x', the first thing cachSolve does is to verify the value 'mat', stored previously 
                              ## with getinvers, exists and is not NULL. If it exists in memory, it simply returns a message and the value 'mat', that is supposed to be the inverse,
                              ## but not necessarily.
                              
        mat <- x$getinverse   ## makeCacheMatrix is a function that stores a list of functions; to use these functions stored in it, we need to subset the main function assigned to x
                              ## beforehand.
        if(!is.null(mat)) {
                message ("getting cache data")
                return(mat)
        }
        data <- x$get()
        mat <- solve(data,...)
        x$setinverse(mat)
        mat
}
