
## Constructor for special matrix object which caches the inverse if calculated
## Creates 4 methods to set and get the values for the matrix and inverse

makeCacheMatrix <- function(x = matrix()) {
	inv_m <- NULL
	
	## redefines matrix and clears inverse cache
    set <- function(y) {
			x <<- y
			inv_m <<- NULL
	}
	
	## returns matrix 
	get <- function() x
	
	## sets cache value for the inverse
	setinv <- function(inv) inv_m <<- inv
	
	## returns the cached value of the inverse
	makgetinv <- function() inv_m
	
	## returns list of methods for the special matrix object
	list(set = set, get = get,
		 setinv = setinv,
		 getinv = getinv)
}


## This function first checks to see if the inverse is cached
## If the inverse is cached, it is returned
## If not, the inverse is calculated, cached, and then returned

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv_m <- x$getinv()
	
	## if inverse is already calculated, return it
	if(!is.null(inv_m)) {
			message("getting cached data")
			return(inv_m)
	}
	
	## else calculate inverse, store inverse, and return value
	data <- x$get()
	inv_m <- solve(data, ...)
	x$setinv(inv_m)
	inv_m
}
