## Our aim in this experiment is to write a pair of functions, namely, 
## "makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix

## makeCacheMatrix is a function which creates a special "matrix" object that can 
## cache its inverse for the input (which is an invertible square matrix)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x                                       #get the value of the Matrix
  setInverse <- function(inverse) inv <<- inverse           #set the value of the invertible matrix
  getInverse <- function() inv                              #get the value of the invertible matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## cacheSolve is a function which computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {                                  #if inverse matrix is not NULL
    message("getting cached result")                   #Type message: Getting Cached Invertible Matrix 
    return(inv)                                        #return the invertible matrix
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
