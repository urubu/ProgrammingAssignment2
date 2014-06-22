## Set of functions for cached computation of matrix inverse
## --
## Coursera R Programming / Assignment 2

## makeCacheMatrix sets up a cache matrix and a list containing functions 
## to get/set the cache matrix and to get/set the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
		
	# set up cache matrix
	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
		
	# set up functions
        get <- function() x
        setSolve <- function(Solve) m <<- Solve
        getSolve <- function() m
		
		# set up function list to return
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}

## cacheSolve calculates the inverse of a matrix by first calling makeCacheMatrix
## to check whether cached inverse already exists. 
## Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        
	# Call getSolve to check if inverse has already been calculated.
	# If so, return cached result.
	m <- x$getSolve()
	if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
		
	# Compute inverse by calling solve
        data <- x$get()
        m <- Solve(data, ...)
	# Cache computed inverse
        x$setSolve(m)
	# Return inverse
        m
}
