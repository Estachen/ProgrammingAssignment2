## The following functions serve to create a matrix, compute its inverse and
## cache it. Upon future recalling of the same matrix, the inverse is returned
## from the cache, skipping the computational steps

## Function creates matrix with a set of four (4) lists

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
}


## Function returns cached matrix
## Computes the inverse of provided matrix if not previously cached

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting inverse matrix")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv  
        ## Return a matrix that is the inverse of 'x'
}
