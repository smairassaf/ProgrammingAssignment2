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
    # to chekc if set was called and matrix was changed, then cached value will be wrong.
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
    # check if matrix wasn't changed
    if(x$ischanged() == FALSE) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
    }
    # if matrix was changed or there is no chached inverse
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
