## This functions are used to cache the inverse of a matrix.
## The idea is to avoid unnecessary computation.

## This function generates a list of 4 items:
## set, get, setinverse and getinverse

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


## Applied on the list of makeCacheMatrix, this function returns the
## inverse of a matrix, returning the cached data if exists or otherwise
## calculating it

cacheSolve <- function(x, ...) {
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
