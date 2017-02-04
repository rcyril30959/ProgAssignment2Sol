## ASSIGNMENT: CACHING THE INVERSE OF A MATRIX
## The following functions are intended to cache potentially time-consuming computations,
##      such as finding the inverse of a matrix. If the nature of the object concerned does
##      not change, then it makes sense to cache the value of the mean rather than recompute
##      each time.

## the makeCacheMatrix function creates a special 'matrix' object that can cache its inverse

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


## The cacheSolve function computes the inverse of the 'matrix' returned by makeCacheMatrix
##      if the inverse has already been computed and the matrix has not changed, then the 
##      inverse will be retrieved from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data, my friend")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
