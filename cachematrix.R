## My purpose is to cache the inverse of a matrix so it can be used
## several times in a program without solving the inverse each time

## The "makeCacheMatrix" function is a list containing functions 
## to set/get matrix and set/get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL # sets the value of i to NULL ("i" is a variable to store matrix inverse )
  setMatrix <- function(y) {
    x <<- y  # caches the input matrix
    i <<- NULL
  }
  getMatrix <- function() x
  setMatrixinverse <- function(inverse) i <<- inverse  # caches the input "inverse matrix" 
  getMatrixinverse <- function() i
  # the output will be a list of 4 functions
  list(setMatrix = setMatrix, getMatrix = getMatrix, 
       setMatrixinverse = setMatrixinverse,
       getMatrixinverse = getMatrixinverse)
}


## The "cacheSolve" function checks if the invese matrix has been calculated before.
## if not, it calculates the inverse matrix

cacheSolve <- function(x, ...) {
  i <- x$getMatrixinverse()  # if an inverse already exists, this function gets it
  if(!is.null(i)) {  #checks if the cacheSolve has been executed before
    message("getting cached data")
    return(i)
  }
  ## if it is the first time that the cacheSolve is called, then:
  data <- x$getMatrix() # gets the matrix
  i <- solve(data, ...) # calculates the inverse
  x$setMatrixinverse(i) # sets the Matrixinverse
  i      
}
