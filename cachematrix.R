## The first function is makeCacheMatrix() - it creates
## a matrix that can cache its inverse.
## The second function is cacheSolve() - it returns the
## the inverse of matrix computed by makeCacheMatrix().
## In case the inverse is already computed and the matrix
## is the same as the created matrix, it'll retrieve the
## inverse of the matrix from cache thereby decreasing the
## computation time.

## makeCacheMatrix returns a list that contains functions to
## 1. Set the value of the matrix.
## 2. Get the value of the matrix.
## 3. Set the inverse of the matrix.
## 4. Get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
inverse1 <- NULL
set <- function(y){
x <<- y
inverse1 <<- NULL
}
get <- function() x
setinverse <- function(inverse) inverse1 <<- inverse
getinverse <- function() inverse1
list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
inverse1 <- x$getinverse()

## Check if inverse has already been computed. If it is
## true, it gets the result. Else, sets the value using
## setinverse function
if(!is.null(inverse1)){
message("getting cached data")
return(inverse1)
}
data <- x$get()
inverse1 <- solve(data)
x$setinverse(inverse1)
inverse1
}

## Test:
## a = rbind(c(-1/2,3),c(3,-1/2))
## b = makeCacheMatrix(a)
## b$get()
##      [,1] [,2]
## [1,] -0.5  3.0
## [2,]  3.0 -0.5
## cacheSolve(b)
##            [,1]       [,2]
## [1,] 0.05714286 0.34285714
## [2,] 0.34285714 0.05714286
## cacheSolve(b)
## getting cached data
##            [,1]       [,2]
## [1,] 0.05714286 0.34285714
## [2,] 0.34285714 0.05714286
