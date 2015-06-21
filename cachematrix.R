## These functions work to cache the inverse of a given
## matrix to reduce number of required cyles

## This function inputs given matrix and creates a list of
## functions to be used in caching of the inverse

makeCacheMatrix <- function(matr = matrix())
{
  # Check input matrix is square and place in array
  numCol <- ncol(matr)
  numRow <- nrow(matr)
  if (numCol != numRow)
  {
    stop("Matrix input is not square")
  }
  ar <- array(NaN, dim = c(numRow, numCol, 2))
  ar[, ,1] <- matr
  
  # function to set new matrix data
  setMatrix <- function(y)
  {
    numCol <<- ncol(y)
    numRow <<- nrow(y)
    if (numCol != numRow)
    {
      stop("Matrix input is not square")
    }
    ar <<- array(NaN, dim = c(numRow, numCol, 2))
    ar[, ,1] <<- y
  }
  
  # function to get original matrix data
  getMatrix <- function()
  {
    ar[, ,1]
  }
  
  # function to input calcultated inverse matrix data
  setInverse <- function(inverse)
  {
    ar[, ,2] <<- inverse
  }
  
  # function to get inverse matrix
  getInverse <- function()
  {
    ar[, ,2]
  }
  
  # output list of functions
  list(
    setMatrix = setMatrix, getMatrix = getMatrix,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


## Calculate or retrive inverse of previously given matrix

cacheSolve <- function(x, ...)
{
  inverse <- x$getInverse()
  if (!is.nan(inverse[1,1]))
  {
    # retrieve inverse and end function
    message("getting cached data")
    return(inverse)
  }
  
  # retrive matrix, calculate inverse and store it
  matrix <- x$getMatrix()
  inverse <- solve(matrix)
  x$setInverse(inverse)
  inverse
}
