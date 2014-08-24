## Matrix inverse computation is a costly operation, these functions/objects will
## allow us to compute matrix inverse as well as store them in cache, so that 
## the next time we need a matrix inverse we can simply get it from cache
## instead of recomputation

## makeCacheMatrix() accept object of type matrix. The functions returns list containing following functions; 
##set() : resets function variables. 
##get() : returns value of the matrix vector
##setsolve() : cached inverse of matrix
##getsolve(): returns the inverse of matrix from cache 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inv <<- solve
  getsolve <- function() inv
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

##cacheSolve() calculates the inverse of matrix using solve() function. The function first check to see if the inverse has already been calculated. 
##If so then it gets the inverse value from cache via getsolve() function and skips the computation. 
##Otherwise it gets the matrix value via get() function then calculate the inverse and sets inverse value in the cache via setsolve() function.    

cacheSolve <- function(x, ...) {
  inv <- x$getsolve()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setsolve(inv)
  inv
}