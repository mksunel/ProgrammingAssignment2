## makeCacheMatrix accept object of type matrix. The fucntion cached the inverse of matrix and returns the inverse matrix.  


## makeCacheMatrix() accept object of type matrix. The functions returns list containing following functions; 
##set() : resets function variables. 
##get() : returns value of the matrix vector
##setsolve() : cached inverse of matrix
##getsolve(): returns the inverse of matrix from cache 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

##cacheSolve() calculates the inverse of matrix using solve() function. The function first check to see if the inverse has already been calculated. 
##If so then it gets the inverse value from cache via getsolve() function and skips the computation. 
##Otherwise it gets the matrix value via get() function then calculate the inverse and sets inverse value in the cache via setsolve() function.    

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}