## These functions cache the inverse of an invertible square matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ## Set the value of the matrix
        set <- function (y) {
                x <<- y
                inv <<- NULL
        }
        ## Get the value of the matrix
        get <- function () x
        ## Set the value of the inverse
        setinverse <- function(solve) inv <<- solve
        ## Get the value of the inverse
        getinverse <- function () inv
        ## Create a list
        list (set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated and the
## matrix has not changed, then cacheSolve will retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        ## Check to see if the inverse has already been calculated
        ## If so, gets the inverse from the cache and skips computation
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        ## Calculates the inverse of the matrix
        inv <- solve (data, ...)
        ## Sets the value of the inverse in the cache via the "setinverse" function
        x$setinverse(inv)
        inv
}
