## Matrix inversion is usually a costly computation and there may be some benefits to caching the inverse of a matrix rather than compute it repeatidly.
## The following two functions allow to cache the inverse of a matrix (whilst assuming that the matrix supplied is always invertible).

## makeCacheMatrix creates a 'special matrix' object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function (x) {
		y<<- x
		inv <<- NULL
	}
	get <- function () y
	set_inverse <- function(inverse) inv <<- inverse
	get_inverse <- function() inv
	list(set=set, get=get, set_inverse=set_inverse, get_inverse=getinverse)
}


## cacheSolve computes the inverse of the 'special matrix' returned by above function makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cashe.

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message ("retrieving cached data")
		return(inv)
	}
	data<- x$get()
	inv<- solve(data)
	x$setinverse(inv)
	inv
}
