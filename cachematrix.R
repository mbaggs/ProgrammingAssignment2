## A pair of functions to find the inverse of a matrix,
## using a cached value when possible to save computation time

## Creates a makeCacheMatrix object from the matrix x, 
## returns a list of functions to set and get stored matrix,
## and to set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Return the inverse of the matrix stored in the makeCacheMatrix x,
## using a cached value if it has been calculated previously

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
