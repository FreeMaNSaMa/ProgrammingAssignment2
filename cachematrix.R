## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
	## makeCacheMatrix creates a list of functions : "set", "get", "setinv" and "getinv"

	## set cached inverted matrix to NULL
	inv_mat <- NULL 

	## this function sets the y argument to x and cached inverted matrix (inv_mat) to NULL
	set <- function(y) 
	{
		x <<- y
		inv_mat <<- NULL
	}

	## get returns the value of x (argument of makeCacheMatrix)
	get <- function() x
	
	## since it as assumed that the matrix is always invertible, invertability won't be checked
	## this function computes the inverse matrix of x and store it in inv_mat 
	setinv <- function(solve) inv_mat<<- solve

	## this function returns the inverse matrix of x 
	getinv <- function() inv_mat

	## returns a list of the functions defined above
	list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
        ## cacheSolve return a matrix that is the inverse of 'x'

	## retrieving the value of inv_mat
	inv_mat <- x$getinv()

	## if the inverted matrix exists, it was cached so displays a message and returns the value cached
	if (!is.null(inv_mat)) 
	{
		message("getting cached data")
		return(inv_mat)
	}

	## if not, set data to x from makeCacheMatrix
	data <- x$get()

	## computes the inverse of x and store it results in inv_mat
	inv_mat <- solve(data)

	## sets inv_mat in x
	x$setinv(inv_mat)

	## returns the inverse of x
	inv_mat

}
