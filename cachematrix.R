makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## Get the matrix
    get <- function() x
    ## Set m value
    setinverse <- function(inverse) m <<- inverse
    ## return m value
    getinverse <- function() m
    ## return the list
    list(get = get, setinverse = setinverse,
         getinverse= getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    ## Use the m value if it's not null
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## calculate matrix inverse
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
