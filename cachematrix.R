##
## Inverting a matrix can be a costly computation, so:
##		makeCacheMatrix - Creates an object containing given a matrix, but also containing space for the matrix's inverse.
##		cacheSolve      - Returns the inverse of a matrix of the previously defined type, from the cache if this operation has already been done.
##

##
## Create an object containing given a matrix, but also containing space for the matrix's inverse
##
makeCacheMatrix <- function(matrixOriginal = matrix())
{
	matrixInverse <- NULL

	set <- function(m)
	{
		matrixOriginal <<- m
		matrixInverse <<- NULL
	}

	get <- function()
	{
		matrixOriginal
	}
	
	setInverse <- function(m)
	{
		matrixInverse <<- m
	}

	getInverse <- function()
	{
		matrixInverse
	}

	list(set = set,
		 get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
}

##
## Return the inverse of a matrix of the previously defined type, from the cache if this operation has already been done
##
cacheSolve <- function(m)
{
	if (is.null(m$getInverse()))
	{
		m$setInverse(solve(m$get()))
	}
	else
	{
		message("getting cached data")
	}

	m$getInverse();
}

