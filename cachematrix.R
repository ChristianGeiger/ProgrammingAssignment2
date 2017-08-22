## Create an object which can store a matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## set the marix to the value of the function argument and set the inverse to NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    ## set the inverse to the value of the function argument
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'.
## Use the cached value if the inverse has already been calculated.
# Argument x has to be  a list containing the fuctions declared in 'makeCacheMatrix'
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
