## Quick overview about the functions
## ==================================
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieve the inverse from the cache.
## It is assumed that the matrix supplied is always invertible.
 
## makeCacheMatrix function
## ========================
## This function creates a special "matrix", which is really a list containing a function to: 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix
makeCacheMatrix <- function(matrixdata = matrix()) {

	# this variable stores inverse of a matrix
	# initializing to null
	inverseMatrix <- NULL
	
	# this function sets the value of the matrix
	set <- function(i) {
		# setting matrix data that exist in "parent" context
		matrixdata <<- i
		
		# since matrix data is changing, 
		# nullifying inverse matrix value
		inverseMatrix <<- NULL
	}
	
	# this function gets the value of the matrix
	get <- function() matrixdata
	
	# this function sets inverse of a matrix that exist in "parent" context
	setinvmatrix <- function(m) inverseMatrix <<- m
    
	# this function gets inverse of a matrix
	getinvmatrix <- function() inverseMatrix
    
	# returning list of functions
	# set & get value of the matrix
	# set and get value of the inverse matrix
	list(set = set, get = get,
		setinvmatrix = setinvmatrix,
		getinvmatrix = getinvmatrix)
}
	
## cacheSolve Function
## ===================
## The following function calculates the inverse of a special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse value from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinvmatrix function.
cacheSolve <- function(cache, ...) {
	# getting inverse matrix value from cache
	m <- cache$getinvmatrix()
    
	# checking whether cached value is null 
	if(!is.null(m)) {
		# since cache value is not null, hence returning cached value
		return(m)
	}
	
	# getting matrix data from cache object
	data <- cache$get()
	
	# computing matrix inverse
	m <- solve(data)
	
	# Setting computed inverse matrix value to the cache
	cache$setinvmatrix(m)
	
	# Returning inverse matrix value
	m
}