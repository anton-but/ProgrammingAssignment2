## The functions perform the following:
## makeCacheMatrix: This function creates a special "matrix" object
##                  that can cache its inverse.
## cacheSolve:      This function computes the inverse of the special "matrix" returned 
##                  by makeCacheMatrix above. If the inverse has already been calculated 
##                  (and the matrix has not changed), then the 
##                  cachesolve should retrieve the inverse from the cache.
## 

## Function "makeCacheMatrix" creates a special "matrix", which is really a list containing a function to:
## 1:   set the value of the Matrix
## 2:   get the value of the Matrix
## 3:   set the value of the Inverse of matrix
## 4:   get the value of the Inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
            s <- NULL
            set <- function(y) {
                    x <<- y
                    s <<- NULL
            }
            get_matrix <- function() x
            setsolve <- function(solve) s <<- solve
            getsolve <- function() s
            list(set = set, get = get,
                 setsolve = setsolve,
                 getsolve = getsolve,)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        ## Checking if calculation already in the cache
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        
        message("No cache for given matrix, calculating inverse matrix")
        
        ## Assign to 'data' value of original matrix 
        data <- x$get_matrix()
        ## Calculation inverse of original matrix
        s <- solve(data, ...)
        ## Save to cache gotten result for future requests  
        x$setsolve(s)
        s
}
