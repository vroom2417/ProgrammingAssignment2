## Put comments here that give an overall description of what your
## functions do
##Varun Ashok Agarwal - First GitHub R Code
## Write a short comment describing this function

## makeCacheMatrix - This function creates a special "Matrix", 
## which is really a list containing a function to - 
#  1.set the value of the matrix
#  2.get the value of the matrix  
#  3.set the Inverse of the matrix
#  4.get the Inverse of the matrix
#  5. The matrix object can cache its own object.

makeCacheMatrix <- function(x = matrix()) { #take the matrix as an input
  invMatrix <- NULL
  
  setMatrix <- function(y) {                #set the value of the Matrix
       x <<- y    #assign a value to an object in an environment that is different
       invMatrix <<- NULL
  }
  
  getMatrix <- function() x                              #get the value of the Matrix
  
  setInverse <- function(inverse) invMatrix <<- inverse  #set the value of the invertible matrix
  
  getInverse <- function() invMatrix                     #get the value of the invertible matrix
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
  
}


## Write a short comment describing this function
## cacheSolve - This function takes the output of makeCacheMatrix(matrix) as an 
## input and checks inverse matrix from makeCacheMatrix(matrix) has any value in it or not.
#   1. In case inverse matrix from makeCacheMatrix((matrix) is empty -
#           it gets the original matrix data and set the invertible  matrix with "solve" function.
#   2. In case inverse matrix from makeCacheMatrix((matrix) has some value in it
#           it returns a message  "Getting Cached Invertible Matrix" and the cached object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  invMatrix <- x$getInverse() #get the value from the makeCacheMatrix function
  if(!is.null(invMatrix)) {                         #if inverse matrix !NULL
      message("Getting Cached Invertible Matrix")    
      return(invMatrix)                             #return the invertible matrix
  }
  
  #if value of the invertible matrix = NULL
  
  MatrixData <- x$getMatrix()                     #get the original Matrix Data 
  invMatrix <- solve(MatrixData, ...)             #inverse the matrix
  x$setInverse(invMatrix)                          
  return(invMatrix)                               
}


