## Put comments here that give an overall description of what your
## functions do

###These two functions creates special matrices, calculates their 
###inverse, and caches the inverted matrices.  The second function 
###then checks through cached matrices, inverting any matrices which 
###have not yet been inverted.  If already inverted, cacheSolve will
###return the cached matrix.

## Write a short comment describing this function

### makeCacheMatrix creates a special matrix then calculates its inverse and caches the result.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve()
        getinv <- function() inv
        list(set = set, get = get, 
             setinv = setinv, 
             getinv = getinv)
}


## Write a short comment describing this function

###cacheSolve calculates the inverse of a matrix created by the makeCacheMatrix function, unless
###the matrix has already been inverted, in which case cacheSolve will return the previously solved 
###matrix which was cached by makeCacheMatrix

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}

