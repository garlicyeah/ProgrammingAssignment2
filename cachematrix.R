## creates a special matrix object that can cache its inverse
## n*n matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	   m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve) 

}


## This computes inverse of special "matrix". If inverse of matrix has already been calculated, it gets the inverse of the matrix from the cache and skips the computation. Otherwise, it calculates the inverse matrix of the data and sets the value of the inverse in the cache via the set.inverse function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$get.inverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set.inverse(m)
        m   

}
