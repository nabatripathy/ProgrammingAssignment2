## Programming Assignment 2: Lexical Scoping
## Write the following functions:
##
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.


###########################################################
# makeCacheMatrix creates a list containing a function to
# 	a. set value of the matrix
# 	b. get value of the matrix
# 	c. set value of "inverse of the matrix"
# 	d. get value of "inverse of the matrix"
###########################################################
makeCacheMatrix <- function(x = matrix()) {
    invrs <- NULL
    set <- function(y) {
        x <<- y
        invrs <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invrs <<- inverse
    getinverse <- function() invrs
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



############################################################################################
## The following function returns the inverse of the matrix. 
## If the inverse has already been computed, it gets the result and skips the computation. 
## Else, computes the inverse, sets the value in the cache by setinverse function.
############################################################################################


# Here we assume the matrix is always invertible. We are not takeing care of the otherwise case.

cacheSolve <- function(x, ...) {
    invrs <- x$getinverse()
    if(!is.null(invrs)) {
        message("getting cached data.")
        return(invrs)
    }
    data <- x$get()
    invrs <- solve(data)
    x$setinverse(invrs)
    invrs
}


## > source("makeCacheMatrix.R")
## > source("cacheSolve.R")
##
#######################

## Test cases for sample run:
## > x = rbind(c(-3, 1/2), c(1/4, -2))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1] [,2]
## [1,] -3.00  0.5
## [2,]  0.25 -2.0
## >
## First run (No cache)
## > cacheSolve(m)
##             [,1]        [,2]
## [1,] -0.34042553 -0.08510638
## [2,] -0.04255319 -0.51063830
## >
## Second run (using cached data)
## > cacheSolve(m)
## getting cached data.
##             [,1]        [,2]
## [1,] -0.34042553 -0.08510638
## [2,] -0.04255319 -0.51063830
## > 

