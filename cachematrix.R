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
  
  ## Check if matrix has changed. If matrix has changed, set inverse to null
  ## as cached inverse is not valid anymore
  set <- function(y){
    if(!identical(x,y)) inv <<- NULL
    x <<- y
  }
  
  get <- function() x
  
  ## Allows to set matrix directly. Caution should be taken to use it directly 
  setInverseMat <- function(mat) inv <<- mat  
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
  ## Inverse has already been calculated. No need to track as inverse is set to null
  inv <- x$getInverseMat()
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  
  ## Calculate Inverse of matrix  
  message("Calculating inverse of a matrix")
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
##  randVal1 <- rnorm(100)
##
##  matObj <- makeCacheMatrix(matrix(randVal1, 10, 10))
##  class(matObj)
##  invMat1 <- cacheSolve(matObj)
##  class(invMat1)
##  invMat2 <- cacheSolve(matObj)
##
##  matObj$get()
##  matObj$getInverseMat()
## 
##  matObj$set(matrix(randVal1,10,10))
##  matObj$getInverseMat()
##  invMat3 <- cacheSolve(matObj)
##
##  set.seed(1)
##  randVal2 <- rnorm(100)
##  matObj$set(matrix(randVal2,10,10))
##  matObj$getInverseMat()
##  invMat4 <- cacheSolve(matObj)
##
####################################################################