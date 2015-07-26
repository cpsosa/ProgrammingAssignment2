## Inverse of a matric=x to introduce the <<- operator 
## which can be used to assign a value to an object 
## in an environment that is different from the current 
## environment. 

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## 1. set the matrix
## 2. get  matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix


makeCacheMatrix <- function(x = numeric()) {
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

## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the 
## inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- X$get()
  m <- solve(data, ...)
  X$setsolve(m)
  m
}
  ## Return a matrix that is the inverse of 'x'
