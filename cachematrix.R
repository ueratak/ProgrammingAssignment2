## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  
    }
  
  get <- function() x
  
  set_inv_mat <- function(solve) m <<- solve
  get_inv_mat <- function() m
  list(set = set, get = get,
       set_inv_mat = set_inv_mat,
       get_inv_mat = get_inv_mat)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  ## Return the inverse of the x matrix
  
  m <- x$get_inv_mat()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
    
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$set_inv_mat(m)
  m
  
}
