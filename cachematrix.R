A pair of functions that cache the inverse of a matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
         inversa <- NULL
         set <- function(y) {
                x <<- y
                inversa <<- NULL
                }
        get <- function() x
        setInverse <- function(inverse) inversa <<- inverse
        getInverse <- function() inversa
        list(set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
               inversa <- x$getInverse()
        if(!is.null(inversa)) {
                message("getting cached data")
                return(inversa)
        }
        data <- x$get()
        inversa <- solve(data, ...)
        x$setInverse(inversa)
        inversa
}
