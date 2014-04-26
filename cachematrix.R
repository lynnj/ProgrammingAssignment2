## Below is a pair of functions that 
## cache the inverse of a matrix.

## The first function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { 
	I <- NULL
	set <- function (y){				## set is a function that set the value of the matrix y
		x <<- y					## when set function is used, a new matrix y is assigned
		I <<- NULL				## reset the value of I
	}
	get <- function()x				## get is a function that can return the special "matrix"
	setinverse <- function(solve) I <<- solve	## setinverse is a function that will find the inverse
	getinverse <- function()I			## getinverse is a function that can return the inverse
	list(set = set, get = get, 			## list is a function that returns all the above 4 functions
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## The second function computes the inverse of the special "matrix" returned by 'makeCacheMatrix'
## IF the inverse has already been calculated, then the 'cachesolve' should retreive the inverse
## from the cache.


cacheSolve <- function(x, ...) { 
            I <- x$getinverse()                      	## query the x matrix's cache
            if(!is.null(I)) {			     	## if there is a cache
                    message("getting cached data")
                    return(I)				## just return the cache, no computation needed
            }
            data <- x$get()				## if there's no cache
            I <- solve(data, ...)			## it is calculated here
            x$setinverse(I)				## save the result
            I 						## Return a matrix that is the inverse of 'x'
}

