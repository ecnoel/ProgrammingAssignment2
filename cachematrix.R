## makeCacheMatrix manages computing matrix x inverse and caching both matrix x
## and its inverse
## cacheSolve orchestrates creating the inverse of matrix x and producing the 
## inverse of matrix x as result. To do so, cacheSolve calls makeCacheMatrix 
## subfunctions defined below.

## makeCacheMatrix uses one argument, matrix x which needs to be inversed
## It invokes solve() to compute the inverse of matrix x argument and caches
## both matrices x and solve(x) 
## makeCacheMatrix returns a list that contains:
## - function set: Resets cached x to matrix x, and cached xsolve to NULL
## - function get: Returns matrix x from cache
## - function setXsolve: Sets cached xsolve to solve(x), this is where solve(x) 
##                       gets invoked
## - function getXsolve: Returns solve(x) from cache, once solve(x) has been 
##                       invoked, this function is what makes it
##                       possible to reuse the inverse of x without invoking 
##                       solve (x) each time

makeCacheMatrix <- function(x = matrix()) {
    
  # Initialize Xsolve
  xsolve <- NULL
    
  # Function set resets cached values:
  #     x set to new input matrix
  #     xsolve set to NULL
  set <- function(y) {
    x <<- y
    xsolve <<- NULL
  }
  
  # Function get returns input matrix x
  get <- function() x
  
  # Function setXsolve sets xsolve cached value to solve(x)
  setXsolve <- function(solve) xsolve <<- solve
  
  # Function getXsolve returns cached value xsolve
  getXsolve <- function() xsolve
  
  # Special "matrix" object returned by makeCacheMatrix
  list(set = set, get = get,
       setXsolve = setXsolve,
       getXsolve = getXsolve)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix
## If the inverse of matrix x is already computed and stored in cache, 
## subfunction getXsolve returns the matrix inverse subsequently returned by
## cacheSolve
## Otherwise the cahed value for matrix x inverse is set to NULL and cacheSolve 
## calls solve() with the cached matrix x as argument to compute matrix x
## inverse. Then cacheSolve calls setXsolve to store in cache the inverse of 
## matrix x. Once done, cacheSolve returns the inverse of matrix x

cacheSolve <- function(x, ...) {
   
  # Xsolve set to solve(x) if results already cached, set to NULL otherwise
  xsolve <- x$getXsolve()
  
  # If solve(x) result already computed and cached, returns cached value 
  # for solve(x)
  
  # Getting solve(x) from cache
  if(!is.null(xsolve)) {
    message("getting cached data")
    return(xsolve)
  }
  
  # Else compute solve(x), inverse of matrix x, then store it in cache 
  
  # Get input matrix x and place in data
  data <- x$get()
  
  # Execute solve(x), inverse of input matrix x (iff matrix x inversible, 
  # otherwise contains an error message)
  xsolve <- solve(data, ...)
  
  # Store solve(x) in cache for future use
  x$setXsolve(xsolve)
  
  # Return solve(x)
  xsolve
}
