## Matrix inversion is usually a costly computation 
##and there may be some benefit to caching the
##inverse of a matrix rather than compute it repeatedly 

## This function creates a special "matrix" object that can cache its invers  

makeCacheMatrix <- function(x = matrix()) {
  
  inver <- NULL
    set <- function(y) {
      x <<- y
      inver <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inver <<- inverse
    getinverse <- function() inver
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }




## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    inver <- x$getinverse()
    if(!is.null(inver)) {
      message("getting cached data")
      return(inver)
    }
    calc <- x$get()
    inver <- solve(calc, ...)
    x$setinverse(inver)
    inver
  }

