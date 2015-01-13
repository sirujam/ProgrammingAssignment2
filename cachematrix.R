###------------------------------------------------------------------------------------##
## The functions makeCacheMatrix() and cacheSolve() show the caching ability of R due  ##
## to lexical scoping and decrease computation time.                                   ##
## Assumption :: The matrix supplied is always invertible (as per Assignment)          ##
###------------------------------------------------------------------------------------##

#########################################################################################
## Description :: Creates a special matrix object that can cache inverse a given matrix##
## Input       :: x -> Matrix is expected                                              ##
##                     Default input is 1x1 matrix with NA                             ##
## Output      :: A special object with 'input matrix' and 'cached inverse matrix'     ##
#########################################################################################
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setInverseMat <- function(solve) inv <<- solve
  getInverseMat <- function() inv
  
  list(set = set, get = get, setInverseMat = setInverseMat, getInverseMat = getInverseMat)
}

###########################################################################################
## Description :: Calculates inverse matrix on an object stored with makeCacheMatrix.    ##
##                If inverse matrix is already calculated, then cached data is returned. ##
## Input       :: x -> Special matrix object created with makeCacheMatrix().             ##
##                ... -> further arguments passed to or from other methods               ##
## Output      :: Matrix containg inverse                                                ##
###########################################################################################
cacheSolve <- function(x, ...) {
  inv <- x$getInverseMat()
  
  # Inverse has already been calculated.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setInverseMat(inv)
  
  return(inv)
}

####################################################################
##  Sample Example
## 
##  source("cachematrix.R")
##  set.seed(111)
##  randVal <- rnorm(100)
##  matObj <- makeCacheMatrix(matrix(randVal, 10, 10))
##  class(matObj)
##  invMat1 <- cacheSolve(matObj)
##  class(invMat1)
##  invMat2 <- cacheSolve(matObj)
##
####################################################################