## The goal is to cache the inverse of a matrix so that R does not need to recompute it because this can be a costly and unnecessary operation if we have already found the inverse. This is especially true as the size of the matrix increases.

## args: x must be an invertible matrix
## returns: a special matrix represented as a list of 4 functions to
##			set and get the matrix, set and get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() {x}
	setinverse <- function(inverse) {i <<- inverse}
	getinverse <- function() {i}
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## args: x is the the special matrix created by makeCacheMatrix
## returns: the inverse of the matrix, computes it only if not in cache
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data,...)
	x$setinverse(i)
	i
}