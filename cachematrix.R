## This file contains a pair of functions that cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL # cached value of the inverse of matrix x
    set <- function(y) { # sets the value for the "matrix" object
        x <<- y
        inv <<- NULL
    }
    get <- function() x # returns the value of matrix x
    setinverse <- function(inverse) inv <<- inverse # caches the value of the inverse of matrix x
    getinverse <- function() inv # returns the cached value inv of the inverse of matrix x
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then
## cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse() # get value of cached inverse of matrix x
    if(!is.null(inv)) { # if there is already a value in the cache, take it
        message("getting cached data")
        return(inv)
    } # else (i.e. if no value for the inverse of matrix x has been calculated yet)
    data <- x$get()
    inv <- solve(data, ...) # caclulate inverse of matrix x
    x$setinverse(inv) # save in che cache
    inv
}
