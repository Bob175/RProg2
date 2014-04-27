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
## \param x    A matrix
## \invariant  The number of columns in x must be a multiple of the number of rows in x.
invertMatrix <- function(x = matrix())
{
  matrixObj <- makeCacheMatrix(x)
  return(cacheSolve(matrixObj))
}

## \brief      makeCacheMatrix creates a function object that can be cached.
## \param x    A matrix (e.g. matrix(1:4, 2))
## \return     Returns a list with four functions that implement operations
##             that can be applied to x.
## \invariant  The number of columns in x must be a multiple of the number of rows in x.
makeCacheMatrix <- function(x = matrix())
{
  invertedX <- NULL
  set <- function(newMatrix) {
    x <<- newMatrix
    invertedX <- NULL  
  }
  get <- function() x
  setInverse <- function(solve) invertedX <<- solve
  getInverse <- function() invertedX
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## \brief      cacheSolve calculates the inverse of a matrix using a function object
##             created by makeCacheMatrix.
## \param x    A matrix function object created by makeCacheMatrix
## \return     Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...)
{
  invertedX <- x$getInverse()
  if (!is.null(invertedX)) {
    message("getting cached data")
    return(invertedX)
  }
  matrixData <- x$get()
  invertedX <- solve(matrixData, ...)
  x$setInverse(invertedX)
  invertedX
}