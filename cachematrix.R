## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly
## The following two methods enable caching of the inverse of a matrix

## Usage:

# > m = matrix(c(1, 2, 3, 4), 2, 2)
# > cm = makeCacheMatrix(m)
# > cm$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(cm)
# NOT getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(cm)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5



## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

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


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      
      message("NOT getting cached data")
      
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}
