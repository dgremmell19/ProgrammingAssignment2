## The makeCacheMatrix will take a matrix and cache it in an alternate
## environment to be used later to call and solve functions quicker. A valid
## matrix must be used for the functions to perform inversion. 

## makeCacheMatrix takes a matrix as its input and stores it in a list of functions.
## Store this outcome to a variable. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## cachesolve takes the list from the alternate function and uses the list functions
## from the alternate environment to invert the matrix. Use the outcome variable from 
## makeCacheMatrix as the input. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
    }
