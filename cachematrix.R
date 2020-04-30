## The two functions included in cachematrix.R will help to cache the inverse of a matrix
## which helps us to prevent repetitive calculation of inverse of the same matrix

## makeCacheMatrix is a function that helps to create an R object 
## that can store a matrix and it's inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve can be used to retrieve the inverse from the cached value
## that is stored in the environment of makeCacheMatrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  matrix <- x$get()
  i <- solve(matrix,...)
  x$setinverse(i)
  i
}
