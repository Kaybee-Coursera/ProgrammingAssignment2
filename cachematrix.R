## makeCacheMatrix sets, gets the value of a matrix while additional 
##functions setinv and getinv sets and gets the inverse matrix.
##cacheSolve function is actually used to calculate the inverse using 
##above function. This program is an example of Lexical Scoping.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inverse <<- solve
  getinv <- function() solve
  list (set = set, get = get, setinv = setinv, getinv = getinv)
  
  
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinv()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinv(inverse)
  inverse
}
##End of Code



