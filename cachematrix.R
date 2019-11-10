## Put comments here that give an overall description of what your
## functions do
## Invering a matrix can be time-consuming, thus in a matrix with fixed contents, caching the value can prevent recomputation.
## This assignment includes two functions, makeCacheMatrix and cacheSolve, to cache the inverse of a matrix.

## Write a short comment describing this function
## The makeCacheMatrix function creates a special "matrix", which is a list containing a function to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list (set = set, 
		get = get, 
		setinverse = setinverse, 
		getinverse = getinverse)
}


## Write a short comment describing this function
## The cachSolve function calculates the inverse of the special "matrix" created by the makeCacheMatrix function. 
## If the inverse has already been calculated, it retrieves the value from the cache without further computation. 
## Otherwise, it calculates the value and sets it in the cache using the setmean function.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv   
}