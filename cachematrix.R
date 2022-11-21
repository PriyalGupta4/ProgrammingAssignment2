## Put comments here that give an overall description of what your functions do
## The function makeCacheMatrix provides a caching option to temporarily store the computed inverse
## of a matrix, so that this compute-intensive task deoesn't need to be performed for every reqeuest. 
## If the inverse doesn't exist, it would create one and store it in the cache for future calls.

## Write a short comment describing this function
## makeCacheMatrix is a function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(inverse_matrix1) m <<- inverse_matrix1
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
  
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}

