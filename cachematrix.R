## will create a cache object of a matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    changed <- FALSE
    set <- function(y) {
        x <<- y
        inv <<- NULL
        changed <<- TRUE
    }
    get <- function() x
    ischanged <- function() changed
    setinverse <- function(inverse) { inv <<- inverse 
    changed <<- FALSE }
    getinverse <- function() inv
    list(set = set, get = get, ischanged = ischanged,
         setinverse = setinverse,
         getinverse = getinverse)
}


## inverse a matrix if none cached or matrix was changed otherwise get from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    if(x$ischanged() == FALSE) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
