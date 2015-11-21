#A pair of functions that cache the inverse of a matrix.

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
#  inv_matrix - sets as NULL when we call function makeCacheMatrix
  inv_matrix <- NULL 
  
  # A setter functions:
    set <- function(new_x) {
    x <<- new_x
    inv_matrix <<- NULL 
  }
  get <- function()          x 
  setInv <- function(inv)    inv_matrix <<- inv 
  getInv <- function()       inv_matrix 
 
  # x <- makeCacheMatrix(testmatrix)
  # x$set(newmatrix) # to change matrix
  # x$get # to get the setted matrix
  # x$setInv # to set the inversed matrix
  # x$getInv # to get the inversed matrix
  list(
    set = set, get = get,
    setInv = setInv,
    getInv = getInv
  )
}



#   cacheSolve: This function computes the inverse of the special "matrix" returned
#   by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed)
#   , then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getInv() 
  
   if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get() 
  m <- solve(data) 
  x$setInv(m) 
  m 
}

