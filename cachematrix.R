## With MakeCachMatrix you can add a matrix to the cache, it will be saved
## as a list. 
## To use it, first create a matrix, then pass this matrix as
## an argument to makeCacheMatrix and store the result (which will be a list)
## in a variable. Call cacheSolve and pass the list in the variable you just
## stored as the argument. The inversed matrix will be returned.

## Saves a matrix in the cache
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- null
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## returns the inversed matrix. When the matrix is already in cache, 
## a message will be displayed. And the matrix will be inversed. If it's
## not in the cache, the matrix will be inversed.
cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}