## The following functions will calculate the inverse of a matrix
## Function number will cache the matrix using the operator <<
## The second function is used for callng makeCacheMatrix

## This function creates a special "matrix" object
## that can cache its inverse.
## It contains a list to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse of the matrix
## 4.  get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	## m will store the inverse of the matrix
           m <- NULL

	## set the initial values of the cached matrix
		   
           set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
	## this function gets the value of the matrix already stored
			
            get <- function() x
	## this function sets the value of the matrix as the inverse

            setInverse <- function(solve) m <<- solve
	## retrives of the value of the matrix 

            getInverse <- function() m
	## define the list of functions
    
			list(set = set, get = get,
                 setInverse = setInverse,
                 getInverse = getInverse)
 
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		 makeCacheMatrix <- function(x, ...) {
            m <- x$getInverse()
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$setInverse(m)
            m
}
