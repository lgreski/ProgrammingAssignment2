## JHU Coursera R Programming Assignment #2
## July 2015
## author: leonard.m@greski.com
##
## Invert a matrix and cache its result, so subsequent requests for matrix inversion
## retrieve from cache rather than redoing the inversion calculation

## makeCacheMatrix() - create a cache for a matrix 

makeCacheMatrix <- function(x = matrix()) {
     ## initialize theInverse to NULL
     theInverse <- NULL
     ## assign contents of input to cached matrix and NULL the inverse
     set <- function(y) {
          x <<- y
          theInverse <<- NULL
     }
     
     ## setup get, setsolve, and getsolve functions to access cache
     get <- function() x
     setsolve <- function(solve) theInverse <<- solve
     getsolve <- function() theInverse
     
     ## create list with methods for get / set of both original matrix
     ## and its inverse, return the list to parent environment.  
     ## note that this technique allows use of $ operator to access
     ## each function from the list
     list(set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)
     
}


## cacheSolve() invert a matrix, or retrieve from cache if it already exists

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     ##
     ## Note: x must be an object of makeCacheMatrix, as I describe 
     ## in my article called makeCacheMatrix() as an Object stored at
     ## https://github.com/lgreski/datasciencectacontent
        
     ## confirm input is a list() with 4 named elements corresponding
     ## to 4 functions for makeCacheMatrix() 
     if ((class(x) != "list") ||
        sum(names(x) %in% c("set","get","setsolve","getsolve")) != 4) {
             message("input must be of type makeCacheMatrix()")
             return(NULL)
     }
     
     ## Attempt to retrieve matrix from cache
     theMatrix <- x$getsolve()
     if (!is.null(theMatrix)) {
          message("getting cached inverse")
          return(theMatrix)
     }
     ## if we get past the if() statement, the cache is empty
     
     ## invert the matrix, set the cache, and return
     data <- x$get()
     if (!is.matrix(data)) {
          stop("object passed to cacheSolve() is not a valid matrix, cannot calculate its inverse.")
     }
     ## check to see whether matrix is invertible, meaning that
     ## the determinant must be non-zero
     if (det(data) == 0) {
          ## can't invert this matrix, so set the cache to NULL
          ## and return
          message("Determinant is zero: matrix not invertible, setting cache to NULL")
          x$setsolve(NULL)
          return(NULL)
     } 
     theMatrix <- solve(data)
     x$setsolve(theMatrix)
     theMatrix
}
