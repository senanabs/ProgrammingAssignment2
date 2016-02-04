## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y  ## assign a value to an object in a different environment
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)  
	
	## return a list containing function to 1. set the matrix, 2. get the matrix, 3. set the inverse, 4. get the inverse 
	
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if(!is.null(inv)) {    ## if the inverse has already been calculated
		message("getting cached data")
		return(inv)    ## return the inverse from the cached 
	}
	
	mat.data <- x$get()
	inv = solve(mat.data, ...)  ## else, calculates the inverse 
	
	x$setinv(inv)  ## sets the value of the inverse 
	return(inv)	   ## return a matrix that is the inverse of 'x'
       
}
