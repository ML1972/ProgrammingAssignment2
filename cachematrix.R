## The calculation of the inverse of a squared matrix is in
## general a computation-intensive procedure, especially for
## large matrices. Hence caching the inverse matrix make sense.
## The following two functions helps to caches the inverse matrix
## of a given matrix

## The function 'makeCacheMatrix' creates a special "matrix" object
## which caches the inverse matrix of the given "matrix" object

makeCacheMatrix <- function(x = matrix())
{
     ## Initial condition for the inverse matrix
     Inverse_M <- NULL
     
     ## Setting the initial conditions of the matrices
     set <- function(y) 
          {
               x <<- y
               Inverse_M <<- NULL
          }

     get <- function() x

     ## Calculation of the inverse matrix     
     set_Inverse <- function(Inv_Mat) Inverse_M <<- Inv_Mat
     get_Inverse <- function() Inverse_M
     
     list(set = set, get = get,
          set_Inverse = set_Inverse,
          get_Inverse = get_Inverse)
     
}


## The function 'cacheSolve' calculates the inverse matrix
## of a matrix 'x'. If the inverse matrix of 'x' already exists
## it returns the cached inverse matrix and exists the function.
## If not the inverse matrix of 'x' will be calculated and caches
## the current inverse matrix

cacheSolve <- function(x, ...) 
{
     ## This function calculates the inverse matrix
     ## of a given matrix 'x'.
     
     Inverse_M <- x$get_Inverse()
     
     ## Check if inverse matrix of the given matrix 'x'
     ## already exists
     if(!is.null(Inverse_M)) 
     {
          message("Getting cached inverse matrix")
          return(Inverse_M) ## Returning cached inverse matrix and exit function
     }
     
     ## Calculation of inverse matrix of given matrix 'x'
     New_Matrix <- x$get()
     Inverse_M <- solve(New_Matrix, ...)
     x$set_Inverse(Inverse_M)
     Inverse_M
}
