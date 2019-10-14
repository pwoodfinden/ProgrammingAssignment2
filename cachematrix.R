## Create a special "matrix" object that can cache its inverse
## Contains 4 elements set, get, setinverse & getinverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function() {
        i <<- solve(x)
    }
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Compute the inverse of the special "matrix" from makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
## then retrieve the inverse from the cache
## Assume the matrix is always invertible
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
