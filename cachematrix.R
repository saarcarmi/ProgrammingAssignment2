## The following functions allow the user to inverse a martrix and cache the result for future use.
## As the invesre calculation could be costly, there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
##
##
## Usage:
##        Given a standard R matrix m, first create a wrapper CacheableMatrix:
##        cacheableMatrix <- makeCacheMatrix (m)
##        Now you can solve the matrix to get its inverse:
##        cacheSolve (cacheableMatrix)
##
##        Additional calls to cacheSolve with the same input, will return the inverse matrix from the cache
##
##        Alternatively you can use cache assign the R matrix later 
##        cacheableMatrix <- makeCacheMatrix()
##        cacheableMatrix$set (m)
##        cacheSolve (cacheableMatrix)
##        
##        The set funciton will also allow to replace the matrix values which is being inverted, e.g:
##        cacheableMatrix <- makeCacheMatrix(m)
##        cacheSolve (cacheableMatrix)
##        cacheableMatrix$set (m1)
##        cacheSolve (cacheableMatrix)
##        
##        in order to get access to the raw matrix you can use
##        cacheableMatrix$get ()


## Function gets a standard R matrix and returns a list of methods to operate the CacheableMatrix (i.e. store values, 
## get solution ). See documetion above for usage information.
makeCacheMatrix <- function(x = matrix()) {
     #cache NULL as the solution by default
     solution <- NULL
     
     set <- function(y) {
          #Store the matrix in the parent environment for caching
          x <<- y
          #Remove previous solution from caching
          solution <<- NULL
     }
     get <- function() x ##return the internal cached matrix
     
     setSolution <- function(sol) solution <<- sol  #store solution in cache. To be used by cacheSolve function 
     
     getSolution <- function() solution  #return solution from cache. To be used by cacheSolve function 
     
     ## return list of functions representing the cacheable matrix object.
     list(set = set, 
          get = get,
          setSolution = setSolution,
          getSolution = getSolution)
}


## This function calculates and returns inverse matrix of cacheable matrix. It will cache the results and will return the result from 
## the cache if it was already calculated.
## The input parameter x, should be a list of type 
cacheSolve <- function(x, ...) {
     #Get previously calculated solution
     sol <- x$getSolution()
     
     #check if the solution was really caculated before, and return it if it was
     if(!is.null(sol)) {
          message("getting cached data")
          return(sol)
     }
     
     #Solution was not calculated before. Get the raw matrix and solve it
     data <- x$get()
     sol <- solve(data, ...)
     
     ##Storee the solution for future use and return it
     x$setSolution(sol)
     sol
}
