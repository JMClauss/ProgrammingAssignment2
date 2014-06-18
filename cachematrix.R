## These two functions are designed to streamline the process of inverting
## potentially large matrices. They do this by creating a cached copy of
## the inverted matrix for use in subsequent computations.

## This function will take the inverse of a matrix and store a cached copy

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## This function will check to see if there is a cached copy of the inverse 
## of the matrix. If so, it returns that cached copy; otherwise it will invert
## the matrix and cache a copy.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
