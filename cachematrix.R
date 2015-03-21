## Functions to cache the inverse of a matrix. 
## It is assumed that the matrix supplied is always invertible.

## Returns a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) 
{
	## 'x' is an invertible matrix
	
	i <- NULL
	
	set <- function(y) 
	{
		## Set to NULL only if matrix changed 
		if (identical(x, y) == FALSE)
		{
			i <<- NULL
		}
		x <<- y
	}
	
	get <- function() x
	setInverse <- function(inverse) i <<- inverse
	getInverse <- function() i
	
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Returns the inverse of special "matrix" object x
cacheSolve <- function(x, ...) 
{
	## 'x' is an invertible special "matrix" object

	i <- x$getInverse()
	
	if (!is.null(i))
	{
		message("getting cached data")
		return(i)
	}
	
	data <- x$get()
	i <- solve(data, ...) 
	x$setInverse(i)
	i
}