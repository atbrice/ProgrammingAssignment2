## The below functions, makeCacheMatrix and cacheSolve cache 
## the inverse of a matrix passed to the makeCacheMatrix
## function.

## The results of makeCacheMatrix can be stored in a List, for example:
## x<-makeCacheMatrix(matrix(c(5,-3,7,-6),nrow = 2,ncol = 2))
## and then recalled by
## cacheSolve(x)

## This function caches the inverse of a matrix passed to it
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function retrieves the cached inverse, or if the value is not stored,
## calculates it
cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
