
##Function makeCacheMatrix creates a list containing a function to:
##-set the value of the matrix
##-get the value of the matrix
##-set the value of the inverse
##-get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y 
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse,
        getinverse = getinverse)
}

##Function cacheSolve:
##-checks if the inverse is already caclulated
##-if so, gets the inverse from cache
##-if not, calculates the matrix inverse & sets the value of the inverse in the cache

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
	    message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
