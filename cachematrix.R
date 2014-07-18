## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     solution <- NULL
     set <- function(y) {
          x <<- y
          solution <<- NULL
     }
     get <- function() x
     setSolution <- function(sol) solution <<- sol
     getSolution <- function() solution
     list(set = set, 
          get = get,
          setSolution = setSolution,
          getSolution = getSolution)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     sol <- x$getSolution()
     if(!is.null(sol)) {
          message("getting cached data")
          return(sol)
     }
     data <- x$get()
     sol <- solve(data, ...)
     x$setSolution(sol)
     sol
}
