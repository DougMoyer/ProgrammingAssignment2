## This package computes the inverse of a matrix.
## It caches the resulting inverse in order to reduce 
## processing time when the inverse needs to be calculated
## multiple times.

## makeCacheMatrix - this function sets up the list of functions
## that can be used to cache the inverse result inverse
##              
## Before using the functions, run the statement:
## source("cachematrix.r") from the command line.
##
## usage: matrixObj <- makeCacheMatrix() 
##
##		OR
##
##	matrixObj <- makeCacheMatrix(matrix(c(.5, -.25, -1.0, .75), nrow = 2, ncol = 2))
##			Creates the object that will cache the
##			original matrix, provide methods to set/change the
##			original matrix, and cache the inverse matrix.
##
##		NOTE: this only needs to be called once. The matrix can be
##			changed any number of times by using the set()
##			function. The getInverse() function will always
##			cache and return the inverse matrix for the last
##			original matrix set().
##
##	matrixObj$get()
##			Gets the original cached matrix.
##			Returns a single element NA matrix if matrix not set.
##
##	matrixObj$set(matrix(c(.5, -.25, -1.0, .75), nrow = 2, ncol = 2))
##			Sets the data matrix in the object. Can be used multiple
##			times to change the cached matrix.
##
##	matrixObj$getInverse()
##			Gets the inverse matrix for the data previously loaded
##			with the set operation above.
##			Returns a single element NA matrix if original matrix
##			not set.
##
##	NOTE: you can also get the inverse matrix by calling the cacheSolve
##		function:
##			cacheSolve(matrixObj)

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL

	set <- function(y) {
		x <<- y
		inv <<- NULL
	}

	get <- function() x

	getInverse <- function(...) {
		if (!is.null(inv)) {
			return(inv)
		}
		else {
			# inverse not cached so compute and cache
			inv <<- solve(x)
			return(inv)
		}
	}

	list(set = set, get = get,
		getInverse = getInverse)
}


## cacheSolve - calculates the inverse of the given matrix.
##			This must be called after the cacheMatrix object is
##			created using the makeCacheMatrix function.
##
## usage: inverseMatrix <- cacheSolve(matrixObj)
##		Returns the inverse matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	x$getInverse(...)
}
