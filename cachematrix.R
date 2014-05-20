##cachematrix.R creates a pair of functions that cache the inverse of a matrix.

## Unit Test
## mySeq <- seq(1:4)
## myMatrix <- matrix(mySeq, 2)
## myMatrix 
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## m = makeCacheMatrix(myMatrix)
## m$get()
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## cacheSolve(m) 
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## cacheSolve(m) 
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL

	get <- function() x
	set <- function(y) {
		x <- y
		i <- NULL
	}

    	inverseGet <- function() i
	inverseSet <- function(z) i <<- z

	list(
		get = get, 
		set = set, 
		inverseGet = inverseGet, 
		inverseSet = inverseSet 
	)
 }


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
##should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
     
	i <- x$inverseGet()
	if(!is.null(i)) {
      	message("getting cached data")
            return(i)
      }
	i <- solve(x$get())
	x$inverseSet(i)
	i
}


