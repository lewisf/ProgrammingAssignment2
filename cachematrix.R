## makeCacheMatrix takes a standard matrix() and creates an environment
## around it that allows one to cache the matrix and its inverse in x
## and m respectively. In order to create a special matrix in y, one would use
## y <- makeCacheMatrix(matrix). In order to change this matrix, they would use
## y$set(newmatrix).
##
## These two functions then use this new special matrix to solve for the inverse
## in a way that keeps the inverse calculation cached. Everytime cacheSolve is
## called, it checks to see if there is an inverse calculation in its cache already.
## If there is, it returns that cached calculation. If not, it calculates it and puts
## it in the cache.

## Make a special matrix represented by a list of functions:
## set, get, setinverse, getinverse. In order to call these functions, one would
## use a subsetting technique: y$set(matrix), y$get(), etc...
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


## cacheSolve takes in an argument(which is expected to be a special matrix created
## by makeCacheMatrix, and tries to get a cached inverse calculation. If it doesn't
## get one, it solves for the inverse and then stores it in the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
