## Cached Matrix Inversion
## The first function, invertMatrix, will compute the inverse of an
## invertible matrix using the 2nd and third functions.

## Example usage:
#>a <- matrix(1:4, 2)
#>a
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4
#>b <- invertMatrix(a)
#>b
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

## \brief      Creates an inverted matrix
## \param m    A matrix
## \invariant  The number of columns in m must be a multiple of the number of rows in m.
invertMatrix <- function(m = matrix())
{
  matrixObj <- makeCacheMatrix(m)
  return(cacheSolve(matrixObj))
}

## \brief      makeCacheMatrix creates a function object that can be cached.
## \param m    A matrix (e.g. matrix(1:4, 2))
## \return     Returns a list with four functions that implement operations
##             that can be applied to m.
## \invariant  The number of columns in m must be a multiple of the number of rows in m.
makeCacheMatrix <- function(m = matrix())
{
  invertedM <- NULL
  set <- function(newMatrix) {
    m <<- newMatrix
    invertedX <- NULL  
  }
  get <- function() m
  setInverse <- function(solve) invertedM <<- solve
  getInverse <- function() invertedM
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## \brief      cacheSolve calculates the inverse of a matrix using a function object
##             created by makeCacheMatrix.
## \param m    A matrix function object created by makeCacheMatrix
## \return     Returns a matrix that is the inverse of m
cacheSolve <- function(m, ...)
{
  invertedM <- m$getInverse()
  if (!is.null(invertedM)) {
    message("getting cached data")
    return(invertedM)
  }
  matrixData <- m$get()
  invertedM <- solve(matrixData, ...)
  m$setInverse(invertedM)
  invertedM
}