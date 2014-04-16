## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix was changed slightly to cache a matrix
## First, it sets the matrix. Then, it inverts and caches

makeCacheMatrix <- function(x = matrix(numeric = 0, 0, 0)) {
        ## numeric = 0 sets an empty numeric matrix
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## solve function inverts the matrix 
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve was changed slightly to solve a matrix
## First, it checks for a cached matrix.
## Then if its cached, its inverts the matrix.

cacheSolve <- function(x, ...) {
        ## m calls for the inverse from x
        ## and checks for cached matrix
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## solve function inverts the matrix
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m        
}
