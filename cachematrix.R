## This function creates a “matrix” object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ##creates a 'matrix' object that can cache its inverse
  m <- NULL #set an empty inverse matrix
  
  set <- function(y) {
    ##sets the value of the matrix (x==y) to the parent env (<<-)
    x <<- y
    m <<- NULL #set an empty inverse matrix in the parent env (<<-)
  }
  
  get <- function() x #gets the value of the matrix
  setInv <- function(inverse) m <<- inverse
  
  ##sets the value of the inverse matrix (m) in the parent env (<<-)
  getInv <- function() m #gets the value of the inverse matrix
  list(set = set, get = get, setInv = setInv, getInv = getInv)

}


## This function computes the inverse of the special “matrix” returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  
  m <- x$getInv() #gets the value of the inverse matrix
  
  if(!is.null(m)) {
    ##if m already exists (the inverse is already calculated)
    message('getting cached data')
    return(m)
  }
  
  # if m doesn't exist, calculate the inverse of the matrix
  data <- x$get() #gets the value of the matrix
  m <- solve(data, ...) #calculates the inverse
  x$setInv(m) 
  
  # return the inverse matrix
  m 
}
