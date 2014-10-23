## makeCacheMatrix take a matrix variable, cacheSovle caches and returns its inverse matrix.

## Function return list of four sub function(set,get,setInverse,getInverse)
## as similar to makeCacheVector set input matrix, get input matrix
## and get and set inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- matrix()
  set <- function(y) {
    x <<- y
    m <<- matrix()
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## This function take a variable that assigned makeCacheMatrix(INPUT_MATRIX_HERE)
## it checks if INPUT_MATRIX_HERE has been inversed and cache, if or inverses it, 
## cache and return its inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!length(m) == 1) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setInverse(m)

  m
}
