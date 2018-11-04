## this program aims to cache the inverse of a matrix


## makeCacheMatrix`: This function creates a special 
## "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  #check if x is a matrix
  if(!is.matrix(x)) {
    stop("x is not a matrix")
  }
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) i <<- inverse
  get_inverse <- function() i
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
  i <- x$get_inverse()
  
  # if there is a cached inverse
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  
  # if there is no cached inverse
  data <- x$get()
  i <- solve(data, ...)
  x$set_inverse(i)
  i
}
# test
a = matrix(c(1,-1,1,1), nrow = 2, ncol = 2)
cacheSolve(makeCacheMatrix(a))