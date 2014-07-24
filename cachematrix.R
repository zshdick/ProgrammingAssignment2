## This R file has two functions to caching the Inverse of a Matrix


## Like the Example, the first function to:
### Set the value of the matrix
### Get the value of the matrix
### Set the value of the inverse
### Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) 
{  
    Ca <- NULL  ##store the cached inverse matrix
    set <- function(y)
      {
	 	x <<- y
       	Ca <<- NULL
       }
    get <- function() x
    seti <- function(solve) Ca <<- solve
    geti <- function() Ca
    list(set = set, 
         get = get, 
         seti = seti, 
         geti = geti)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
###If the inverse has already been calculated (and the matrix has not changed), 
###then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) 
{
    Ca <- x$geti()
    if (!is.null(Ca)) 
      {
       	message("getting cached data")
       	return(Ca)
       }
    data <- x$geti()
    Ca <- solve(data, ...)
    x$seti(Ca)
    Ca
}