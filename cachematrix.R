## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix acts as a 
## store for a square matrix and its inverse.  The point 
## of the function is to prevent the computation of the 
## inverse of the matrix multiple times (if many solves are required).
## makeCacheMatrix returns a list of functions (set, get, getinv, setinv)
## to respecively set and get the matrix and set and get the inverse of the
## matrix.

## cacheSolve is called to initially set the value of the inverese. One can solve 
## Ax = b in the following ways using these funtions.
## z = makeCacheMatrix(A)
## x = cacheSolve(z) %*% b
## or if cacheSolve has already been called for z then
## x = z$getinv() %*% b
##
##This assumes that A is square, invertible, and that x and b are conformable. 


## Write a short comment describing this function

## makeCacheMatrix accepts a single (square and invertible) matrix as in input.
## There is no error checking.
## The output of this function is a list of functions with the "names" 
## (get, set, getinv, setinv)



makeCacheMatrix <- function(x = matrix()) {
      ## makeCacheMatrix stores the input matrix x
      ## and in addition stores the inverse of x after
      ## a call to cacheSolve
  
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(invcomp) inv <<-invcomp  
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

## The input for this function is the output of makeCacheMatrix
## The output for this function is the inverse (computed by the R function solve) of 
## the matrix input to makeCacheMatrix.  More importantly cacheSolve sets the value of 
## the inverse matrix so that it can be accessed through the getinv option in 
## the makeCacheMatrix above.  
##
## There is no error checking for this function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## 'x' is the return value of makeCacheMatrix
        ## cacheSolve also "sets" the inverse value so that
        ## it can be retrieved by x$getinv()
  
      inv <- x$getinv()
      if(!is.null(inv)){
          message("getting cached data")
          return(inv)
      }
      d  <- x$get()
      inv <- solve(d, ...)
      x$setinv(inv)  ##Set the inveres value of x
      inv
}
