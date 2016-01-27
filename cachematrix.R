## Provide a function to create a wrapped matrix object that can hold a cached inverse
## and a function to calculate the inverse if not cached yet, or return the cached inverse
## otherwise.  The set function can be used to reset a new matrix to the wrapped matrix object.
##
## Example:
##
## > a <- matrix(c(4,3,3,2), 2, 2)
## > a
##      [,1] [,2]
## [1,]    4    3
## [2,]    3    2
## > b <- makeCacheMatrix(a)
## > cacheSolve(b)
##      [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4
## > cacheSolve(b)
## getting cached data
##      [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4
## > b$set(matrix(c(1,0,0,1), 2, 2))
## > cacheSolve(b)
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1
## > cacheSolve(b)
## getting cached data
##      [,1] [,2]
## [1,]    1    0
## [2,]    0    1
## > 



## Create a wrapped matrix object that can hold a cached inverse
## x is a square invertible matrix

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL

	# set is NOT used by the cacheSolve, but is good to offer. 
	set <- function(newMatrix) {
		x <<- newMatrix
		inverse <<- NULL
	}
	get <- function() x
	setInverse <- function(newInverse) inverse <<- newInverse
	getInverse <- function() inverse
	list(set = set, get = get, 
		setInverse = setInverse,
		getInverse = getInverse)
}


## Return a matrix that is the inverse of 'x'
## Will grab from cache if available
## x must be a wrapped matrix created by makeCacheMatrix
## ... are extra arguments to be passed to the solve() function
cacheSolve <- function(x, ...) {
	inverse <- x$getInverse()
	if (!is.null(inverse)) {
		message('getting cached data')
		return(inverse)
	}
	data <- x$get()
	inverse <- solve(data, ...)
	x$setInverse(inverse)
	inverse
}
