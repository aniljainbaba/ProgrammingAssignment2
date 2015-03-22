## This assignment is about caching the inverse of a Matrix
#author - Anil Kumar Jain
#Date - 22 Mar 2015
## functions do


## How to test this assignment, follow the below steps
#Step 1 Load this file using command source"cachematrix.R")
#Step 2 create square matrix say m1<-matrix(c(5,4,3,2,2,2,7,8,1),3,3)
#Step 3 Call the function makeCacheMatrix and pass matrix m1 as argument. Assign the function call to another variable mat1  => mat1 <- makeCacheMatrix(m1)
#Step 4 Call the function cacheSolve(mat1) => cacheSolve(mat1)
#Step 5 you should see the belowresult
#       [,1]  [,2]   [,3]
#[1,]  0.875 -0.75 -0.125
#[2,] -1.250  1.00  0.750
#[3,] -0.125  0.25 -0.125



## Write a short comment describing this function
##This function makeCacheMatrix creates a special "matrix" object that can cache its inverse.
#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse
#4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
 i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) i <<- solve
    getmatrix <- function() i
    list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
 
}


## Write a short comment describing this function
##This function cacheSolve  computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and #the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i<- x$getmatrix()
	#If the inverse of  matrix available in the environment i.e. cache, then just retrieve it and return
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
	#Else get the data of the matrix, calculate the inverse of it, and set it to environment 
    data <- x$get()
    i<- solve(data, ...)
    x$setmatrix(i)
    i
}
