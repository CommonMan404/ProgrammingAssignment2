## These functions together calculate the inverse for a matrix (if possible)
## and store the result (and some functions) in a list. 

## This function initialises the Matrix x, the object m 
## (where the inverse will be stored), and the set and get
## functions that store and retrieve the data
## as an example of how the code works try:
## > x <- matrix(c(2,2,3,2),nrow=2,ncol=2)
## > MyInverse <- makeCacheMatrix(x
## > cacheSolve(MyInverse)
## and cacheSolve(MyInverse) again to see that it retrieves it from cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}




## This is the function that solves the inverse for the matrix and 
## and retrieves it if it has been solved before. If it is a new matrix
## it stores the results is m and calls the setinverse function
## that stores the result in the list produced by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
