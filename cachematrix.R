## JHU Coursera R Programming Assignment #2
## July 2015
## author: leonard.m@greski.com
##
## Invert a matrix and cache its result, so subsequent requests for matrix inversion
## retrieve from cache rather than redoing the inversion calculation

## makeCacheMatrix() - create a cache for a matrix 

makeCacheMatrix <- function(x = matrix()) {
     ## confirm input is a square matrix
     if(!isValidMatrix(x)) {
             stop("input is not a valid matrix")
             return(NULL)
     }
     ## initialize theInverse to NULL
     theInverse <- NULL
     ## assign contents of input to cached matrix and NULL the inverse
     set <- function(y) {
         ## confirm input is a square matrix
         if(!isValidMatrix(y)) {
                 stop("input is not a valid matrix")
                 return(NULL)
         }
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
     ## if we get past the if() statement, the cache is empty, so
     ## invert the matrix, set the cache, and return
     data <- x$get()
     theMatrix <- solve(data)
     x$setsolve(theMatrix)
     theMatrix
}

isValidMatrix <- function(x = matrix()) {
        # validate as square matrix
        if (!is.matrix(x) || nrow(x) != ncol(x)) {
                stop("object passed to makeCacheMatrix() is not a square matrix, object not initialized.")
        }
        if(det(x) == 0) stop("Determinant is zero: matrix not invertible, object not initialized")
        # if we get this far, matrix is square and invertible and therefore
        # can be inverted and cached 
        return(TRUE)
}