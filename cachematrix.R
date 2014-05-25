## The makeCacheMatrix function is a function that caches the inverse of a matrix 
## if it has already been calculated. It also stores the matrix if it was previously given
## that information. The cacheSolve calculates the inverse of the matrix if it has not already 
## been calculated and caches the answer. If it has been calculated it returns the value of 
## the cached calculation. 

## The fucntion below is used to get or retrieve information about the cached matrix.
## It has 4 sub-functions (GetMatrix, SetMatrix, GetInverseMatrix, SetInversematrix). 
## you should allow pass a square matrix to it.
makeCacheMatrix <- function(x = matrix()) {
	
	CachedInverseMatrix <- NULL
	
	SetMatrix <- function(InputMatrix) {
		x <<- InputMatrix
		CachedInverseMatrix <<- NULL
	}

	GetMatrix <- function() {
		x
	}

	
	GetInverseMatrix <- function() {
		CachedInverseMatrix
	}


	SetInverseMatrix <- function(PrecalculatedInverseMatrix) {
		CachedInverseMatrix <<- PrecalculatedInverseMatrix
	}
	

	list(SetMatrix = SetMatrix,
	     GetMatrix = GetMatrix, 
             GetInverseMatrix = GetInverseMatrix, 
             SetInverseMatrix = SetInverseMatrix) 

}


## The cacheInverseMatrix calculates the inverse of matrix x if it has not been calculated and
## caches the result. If it has been calculated it returns the cached value.
cacheSolve <- function(x, ...) {
        CachedInverseMatrix <- x$GetInverseMatrix()
	if (!is.null(CachedInverseMatrix)) {
		message("getting cached data")
		return(CachedInverseMatrix)
	}
	
	InputMatrix<- x$GetMatrix()
	CachedInverseMatrix <- solve(InputMatrix)
	x$SetInverseMatrix(CachedInverseMatrix)
	CachedInverseMatrix
}
