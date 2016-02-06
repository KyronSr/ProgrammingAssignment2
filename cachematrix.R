## Programming Assignment Number 2 - Lexical Scoping
## Shows how to cache stuff by taking advantage of scoping rules
## basically, ends up more than likely storing this in the global environment
## note, I think this way of doing it is wack... I've created my own object, makeMatrixInverser, 
## below which hides the implementation details from the end-user

## creates the special object
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## performs the solve function, but uses a cache if it is there
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

## creates an object which can optionally use a cache
makeMatrixInverser <- function(x=matrix(), useCache = TRUE){
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  getinverse <- function() {
    if (useCache){
      if(!is.null(m)) {
        message("getting cached data")
        return(m)
      }
      m <<- solve(x)
      m
    } else {
      solve(x)
    }
    
  }
  list(set = set, get = get,
       getinverse = getinverse)
}
