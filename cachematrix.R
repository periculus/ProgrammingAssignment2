## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # Varible that stores the inverse, if it has been calculated
  inv <- NULL
  
  # set method, that sets the matrix, and flags that no inverse has been calculated
  # note that using the <<- operator the parent environment, of the makeCacheMatrix 
  # function, is used
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get method that returns the matrix, it finds x in the parent environment
  get <- function() x
  
  # set method for the cached inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # get method for the cached inverse
  getInverse <- function() inv
  
  # the return value is a list of these methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The cacheSolve finds the inverse of the matrix (stored in) x, and saves it
## in a cache in x.
## Arguments '...' are passed on and it is important that one of those arguments is not
## 'a' or 'b' for solve, then the behaviour of the method is not defined
cacheSolve <- function(x, ...) {
  # Get the inverse of x as stored
  inv <- x$getInverse()
  
  # If it is not NULL, it has been computed and we can return the cached value
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise ...
  # Get the actual matrix
  mat <- x$get()
  
  # Compute its inverse, assuming it exists
  # As the template allows for other arguments ('...'), care needs to be taken that these are not
  # the 'a' or 'b'argument, then 'solve' is not guaranteed to return the inverse of mat. 
  inv <- solve(mat, ...)
  
  # Store this value in the cache
  x$setInverse(inv)

  ## Returns a matrix that is the inverse of 'x'  
  inv  
 }
