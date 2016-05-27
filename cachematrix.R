# makeCacheMatrix
# Creation of a special matrix
# cacheSolve function creates the inverse of the matrix
# If the inverse matrix has already been cached, it isn't necessary to create it again, returning it from the cache

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  set_inverse<- function(inverse) inv_x <<-inverse
  get_inverse <- function() inv_x
  list(set = set, 
       get = get, 
       set_inverse = set_inverse, 
       get_inverse = get_inverse)
}


# cacheSolve
# Returns the inverse of a matrix created using makeCacheMatrix function
# If the inverse is already cached, cacheSolve retrieves it
# if the inverse is not present, the inverse matrix is created, cached and returned

cacheSolve <- function(x, ...) {
  inv_x <- x$get_inverse()
  if (!is.null(inv_x)) {
    message("getting the inverse matrix cached!")
    return(inv_x)
  } else {
    inv_x <- solve(x$get())
    x$set_inverse(inv_x)
    return(inv_x)
  } 
}