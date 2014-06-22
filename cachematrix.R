## A pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInvert <- function(invert) m <<- invert
        getInvert <- function() m
        list(set = set, get = get,
             setInvert = setInvert,
             getInvert = getInvert)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getInvert()
	if(!is.null(i)){
		message("getting cached invert matrix")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setInvert(data)
	i
}
