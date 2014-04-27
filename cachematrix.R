## The two functions are used to find the inverse of a matrix. The first stores it in cache and the second checks
## whether it is in the cache before it finds it itself.

## This function creates a inverse of a matrix and stores it in Cache Memory

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)


}


## This function first checks if the inverse of the matrix is in the cache. If it is it pulls it from there.
## If the inverse is not in cache it goes ahaid and finds it

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
        ## Return a matrix that is the inverse of 'x'
}
