## This code allows for the creation of a special matrix with cachable inverse

## This function takes a matrix and returns a special matrix representation, with a list of functions to manipulate its internal representation.

makeCacheMatrix <- function(x = matrix()) {
    c_inv <- NULL
    set <- function(y) {
            x <<- y
            c_inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) c_inv <<- inverse
    getinverse <- function() c_inv
    list(set = set, 
    	 get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function takes a special matrix representation, calculates and caches its inverse and returns its inverse.

cacheSolve <- function(x, ...) {
    c_inv <- x$getinverse()
    if(!is.null(c_inv)) {
            message("fetching cached inverse")
            return(c_inv)
    }
    data <- x$get()
    c_inv <- solve(data, ...)
    x$setinverse(c_inv)
    c_inv
}
