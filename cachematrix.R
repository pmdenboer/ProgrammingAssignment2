## These functions create a special matrix object that can cache
## its inverse and retrieve the cached inverse when available.

## Create a matrix object that can store its inverse
makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL

    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    get <- function() x

    setinverse <- function(inverse) {
        inv <<- inverse
    }

    getinverse <- function() {
        inv
    }

    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}

## Compute or retrieve the cached inverse of a matrix
cacheSolve <- function(x, ...) {

    inv <- x$getinverse()

    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)

    inv
}
