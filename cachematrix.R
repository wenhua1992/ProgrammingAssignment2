## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix: This function creates a special object that can cache the inverse of the input matrix.
## cacheSolve: The input is makeCacheMatrix object. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache of this makeCacheMatrix object.
## Example:
## x<-cbind(c(2,0),c(0,1))
## y<-makeCacheMatrix(x)
## cacheSolve(y) # Calling this function for the first time will calculate the inverse of x.
## cacheSolve(y) # This time it will print message "getting cached data"







## Write a short comment describing this function

## makeCacheMatrix creates a list of functions which:
## 1. set: set the value of the matrix
## 2. get: get the value of the matrix
## 3. setinv: set the value of the inverse
## 4. getinv: get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL;
	set <- function(y) {
      	x <<- y
      	inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
          	getinv = getinv)

}


## Write a short comment describing this function
## calculates the inverse of special "matrix" returned by makeCacheMatrix above. 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	inv <- x$getinv()
	## If inv is not null, it means that the inverse of x has been calculated. We return what's stored in cache.
      if(!is.null(inv)) {
      	message("getting cached data")
            return(inv)
      }
	## Else if inv in null, then inverse of x has not been calculated. We use solve() to calculate the inverse.
      temp <- x$get()
      inv <- solve(temp, ...)
      x$setinv(inv)
      inv
}
