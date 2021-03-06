# approach 1: create a matrix object, then use it as input to cacheSolve()

a <- makeCacheMatrix(matrix(c(-1, -2, 1, 1), 2,2))
b <- cacheSolve(a)
c <-matrix(c(-1, -2, 1, 1), 2,2) 
d <- solve(c)
identical(b,d)

# call cacheSolve(a) a second time to trigger the "getting cached inverse" message
cacheSolve(a)

# multiply the matrix by inverse, resulting in identity matrix
# check by comparing to result from diag() function 
identical(a$get() %*% a$getsolve(),diag(x=1,nrow(a$get()),ncol(a$get())))

# reset a with another matrix to clear out cached value
a$set(matrix(c(2,3,2,2),2,2))

# confirm that a has new data and that cache is NULL
a$get()
a$getsolve()

# rerun cache solve, note that "getting cached inverse" does not print,
# and that we get a different result
cacheSolve(a)

# now return "getting cached inverse" message
cacheSolve(a)

# approach 2: use makeCacheMatrix() as the input argument to cacheSolve()
#             note that the argument to cacheSolve() is a different object
#             than the argument to the first call of cacheSolve()
cacheSolve(makeCacheMatrix(matrix(c(-1, -2, 1, 1), 2,2)))

# try a non-invertible matrix
rm(b)
b <- makeCacheMatrix(matrix(c(0,0,0,0),2,2))


# illustrate getting the memory locations
a <- makeCacheMatrix(matrix(c(-1, -2, 1, 1), 2,2))
tracemem(a)
tracemem(matrix(c(-1, -2, 1, 1), 2,2))

# approach 2: use makeCacheMatrix() as the input argument to cacheSolve()
#             note that the argument to cacheSolve() is a different object
#             than the argument to the first call of cacheSolve()
cacheSolve(makeCacheMatrix(matrix(c(-1, -2, 1, 1), 2,2)))

# illustrate getting the memory locations
a <- makeCacheMatrix(matrix(c(-1, -2, 1, 1), 2,2))
tracemem(a)
tracemem(matrix(c(-1, -2, 1, 1), 2,2))

# test non-matrix input: should return "not a matrix" error

a$set(1:5)

# test an input that is not an object of type makeCacheMatrix()
# should return "input must be of type makeCacheMatrix()"
cacheSolve(matrix(c(-1, -2, 1, 1), 2,2))

## test non-square matrix
z <- makeCacheMatrix(matrix(c(-1, -2, 1, 1,4,5), 3,2))
cacheSolve(makeCacheMatrix(matrix(c(-1, -2, 1, 1,4,5), 3,2)))

## test non-invertible matrix
z <- makeCacheMatrix(matrix(c(0,0,0,0),2,2))


