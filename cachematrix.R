## makeCacheMatrix() function takes a matrix argument as its input.  Initially 
## the input matrix is stored in the get() function.  The output is simply a 
## list containing the "pointers" to the get(), set(), getinverse(),
## setinverse() functions.

## The remaining functions (set, setinverse, getinverse) inside the 
## makeCacheMatrix() environment are "updated" by lexical scooping via the  
## cacheSolve() function.  When an object is defined by makeCacheMatrix()
## it points to the functions within the makeCacheMatrix environment and are
## defined when the object name is the argument in the cacheSolve() function.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve(x)
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse, 
       getinverse = getinverse)
}

## Write a short comment describing this function

## Ultimately, cacheSolve() function will invert the matrix provided
## in the argument.  If the matrix is newly defined via makeCacheMatrix() 
## then it will print the inverse matrix.  If the argument is an object
## that has been already been defined in the setinverse() function,
## as it is stored in memory, then the stored matrix is retrieved 
## from x$getinverse() and will print the "getting cache matrix" before
## giving the inverse of the matrix.

## Note: only invertible matrix will work.

cacheSolve <- function(x = matrix(), ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cache matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  ## Return a matrix that is the inverse of 'x'
}