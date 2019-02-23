## These functions create a time saving way of looping the inversion of a matrix
## so that it can be repeated quickly.

## This function creates a "special" matrix which can have its inverse cached.

makeCacheMatrix <- function(x = matrix()) {
      minv = NULL
      set <- function(y) {
        x <<- y
        minv <<- NULL
      }
      get <- function() x
      setm <- function(solve) minv <<- solve
      getm <- function() minv
      list(set = set, get = get, setm = setm, getm = getm)
}


## This function calculates the inverse of the matrix produced by the above function.
## It seeks to use the cahced inverse or to save the inverse to the cache for future 
## computations.

cacheSolve <- function(x, ...) {
        minv <- x$getm()
        if(!is.null(minv)) {
          message("getting cached data")
          return(minv)
        }
        data <- x$get()
        minv <- solve(data, ...)
        x$setm(minv)
        
        minv
}

##Test run of the functions on a matrix 'myM'
myM<- matrix(rnorm(9), nrow = 3, ncol = 3)
myMa <- makeCacheMatrix(myM)
cacheSolve(myMa)
##results from test on myM
## [,1]       [,2]      [,3]
## [1,]  0.34194713  0.8549975 0.5987029
## [2,] -0.42927732 -0.5476019 0.4883472
## [3,] -0.05311462 -0.9316392 0.6094987
