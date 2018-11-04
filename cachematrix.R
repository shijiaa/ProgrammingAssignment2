## this program aims to cache the inverse of a matrix


## makeCacheMatrix`: This function creates a special 
## "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  #check if x is a matrix
  if(!is.matrix(x)) {
    stop("x is not a matrix")
  }
  
  inverse_x <- NULL
  set <- function(y) {
    x <<- y
    inverse_x <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) inverse_x <<- inverse
  get_inverse <- function() inverse_x
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## `cacheSolve`: This function computes the inverse of the special "matrix" 
## returned by `makeCacheMatrix` above. If the inverse has already been
## calculated (and the matrix has not changed), then `cacheSolve` 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse_x <- x$get_inverse()
  
  # if there is a cached inverse
  if(!is.null(inverse_x)) {
    message("getting cached inverse")
    return(inverse_x)
  }
  
  # if there's no cached inverse
  data <- x$get()
  inverse_x <- solve(data, ...)
  x$set_inverse(inverse_x)
  inverse_x
}
# test
a = matrix(c(1,-1,1,1), nrow = 2, ncol = 2)
cacheSolve(makeCacheMatrix(a))