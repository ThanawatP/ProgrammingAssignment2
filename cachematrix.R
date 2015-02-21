## Put comments here that give an overall description of what your
## functions do
## First, we need a variable that can store a matrix and cache its inverse.
## Then we can use a list to store data above via get and set function.
## The point is using the <<- operator which can be used to assign a value to an object 
## in an environment that is different from the current environment.
## After that we use cacheSolve to find the inverse of the matrix returned by makeCacheMatrix.
## If there is the inversed matrix stored in the list, the cacheSolve will return that 
## inversed matrix. But if there isn't, the cacheSolve will caculate and set in the list.

## Write a short comment describing this function
# Function makeCacheMatrix creates a matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
# Function cacheSolve computes the inverse of the matrix returned by makeCacheMatrix function.
# If the inverse has already been calculated (Already set by setInverse()),
# then the cacheSolve should retrieve the inverse from cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
