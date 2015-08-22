## This function creates a matrix object that can cache its inverse.
## As illustrated  in the assignment instructions, for this assignment, I am  assuming that
## the  matrix supplied  is always  invertible.

makeCacheMatrix <- function(x = matrix()) {
	
	i <- NULL
	set <- function (y)
	    {
		x <<- y
		i <<- NULL
	     }
	get <- function () x
	setinverse<-function(inverse)  i<<- inverse
	getinverse<-function() i
       list(set = set,
                   get = get,
                   setinverse = setinverse.
                   getinverse = getinverse
              )


}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
                                                    ## Return a matrix that is the inverse of 'x'
 i<- x$getinverse()              
        if(!is.null(i)){	                    ## Check  if the matrix has already been  cached
        	message("getting cached data")      ## if cached return the cached value
        	return (i)
        }
        matrix <- x$get()	                     ## Else get the inverse of the matrix
        i<- solve(matrix,...)                        ## Computing the inverse of a matrix  
        x$setInverse(i)
      return(i)			                     ## return the inverse

}
