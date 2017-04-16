## It's a pair of functions that cache the inverse of a matrix

## This function creates a 'matrix' object, so it can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  get<-function()x
  setInverse<-function(inverse) inverse<<-inverse
  getInverse<-function()inverse
  list(set=set,
       get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}

## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse<-x$getInverse()
  if(!is.null(inverse)){
    message("get cached data")
    return(inverse)
  }
  matrix<-x$get()
  inverse<-solve(matrix, ...)
  x$setInverse(inverse)
  inverse
}
