## Cache the inverse of a given matrix

## The first function returns a list of functions:
## 1. Set up the matrix
## 2. Get the matrix
## 3. Set the inverse of the given matrix
## 4. Get the inverse of the given matrix

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  setmatrix <- function(y){
    x <<- y
    Inv <<- NULL
  }
  getmatrix <- function() return(x)
  setinverse <- function(inverse) Inv <<- inverse
  getinverse <- function() return(Inv)
  # return a list of the four functions
  list(setmatrix = setmatrix, getmatrix = getmatrix, 
       setinverse = setinverse, getinverse = getinverse)
}


## The second function calculates the inverse matrix of the matrix given above.
## If the inverse matrix has already been calculated and the given matrix has not changed,
## retrieve the stored inverse from the cache 

cacheSolve <- function(x, ...) {
  ## x is the output (list of functions) of the first function
  Inv <- x$getinverse()
  # If the inverse matrix has already been calculated by the first
  # function and stored in cache, retrieve it.
  if(!is.null(Inv)) {
    message("Retrieve stored inverse matrix ...")
    return(Inv)
  }
  # Otherwise calculate the inverse matrix and store it in the cache
  matrix <- x$getmatrix()
  Inv <- solve(matrix,...)
  x$setinverse(Inv)
  return(Inv)
}
