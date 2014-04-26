## cachematrix.R contains functions to cache support of matrix inversion

## makeCacheMatrix is encapsulation of matrix with its inversion
## input : matrix
## output : list of operations
##  $get(x) - get matrix
##  $set(x) - set matrix
##  $getInversion(x) - get inversion of matrix
##  $setInversion(x) - set inversion of matrix

makeCacheMatrix <- function(x = matrix()) {
  ## throw error if matrix is not square
  if(ncol(x) != nrow(x)){
    stop("matrix has to be square")
  }
  inversion <- NULL
  set <- function(y) {
    x <<-y
    inversion <<- NULL
  }
  get <- function() x
  setInversion <- function(i) inversion <<- i
  getInversion <- function() inversion
  list(set = set, get = get, 
       setInversion = setInversion, getInversion = getInversion)
}

## Write a short comment describing this function
## cacheSolve resolves getting of matrix inverse 
## input : cacheMatrix (created with function above)
## output : inversion of matrix
cacheSolve <- function(x, ...) {        
  inversion <- x$getInversion()
  if(!is.null(inversion)){
    return(inversion)   
  }
  inversion <- solve(x$get())
  x$setInversion(inversion)
  inversion
}
