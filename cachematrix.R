## These functions calculate the inverse of a matrix
## and store the result in cache

## to execute, run makeCacheMatrix on the desired matrix
## and store the output as an object. 
## Then run cacheSolve with the output of makeCacheMatrix
## as the argument

## The first function creates a list of functions
## to set and retrieve the input matrix and the inverse

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(mean) m <<- mean
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The second function calculates and returns the 
## inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            m <- x$getinv()
        ## if m is already stored in cache,
        ## return value of m and exit function
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
