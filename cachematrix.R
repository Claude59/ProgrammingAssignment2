## ***************************************************************************************************************
##
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a  
## matrix rather than computing it repeatedly (there are also alternatives to matrix inversion that we will not  
## discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.
##
## Write the following functions:
##     
##  1 - makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##
## ***************************************************************************************************************


makeCacheMatrix <- function(mat = matrix()) {
    
     matInv <- NULL
     
## set is a function that changes the matrix stored in the main function    
     set <- function(y) {
          mat <<- y
          matInv <<- NULL
     }
     
## Functions for getting  matrix value
     get <- function() mat

## Inversing the matrix using build in solve() function
 ## fonction setsolve
     setsolve <- function(solve) matInv <<- solve

 ## fonction Getsolve
     getsolve <- function() matInv

## the following line stores the 4 functions: set, get, setsolve and getsolve    
     list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
     
}


## ***************************************************************************************************************
##
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should 
## retrieve the inverse from the cache.
##
## ***************************************************************************************************************



cacheSolve <- function(x, ...) {

## Return a matrix that is the inverse of 'x'
     matInv <- x$getsolve()

## to verify the value matInv (Inversed matrice), stored previously with getsolve, exists and is not NULL     
     if(!is.null(matInv)) {
          message("getting cached data")
          return(matInv)                    ## return the existing value of inversed matrix in cache
     }
     mat <- x$get()                         ## retrieves the matrix to reverse
     matInv <- solve(mat, ...)              ## calculating the inverse matrix
     x$setsolve(matInv)                     ## put the inversed matrix in cache
     matInv                                 ## return the inversed matrix
}





## ***************************************************************************************************************
##  matrix
##
##        [,1] [,2] [,3]
##  [1,]    2   -4    4
##  [2,]    2    0    1
##  [3,]    4    1    1
##
## > cacheSolve(makeCacheMatrix(matrix(c(2,2,4,-4,0,1,4,1,1), nrow=3, ncol=3)))
## 
##       [,1] [,2] [,3]
##  [1,]  0.5   -4    2
##  [2,] -1.0    7   -3
##  [3,] -1.0    9   -4
##
##  = inversed matrix
##
## ***************************************************************************************************************

