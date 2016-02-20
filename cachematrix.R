## The following two functions can be used to compute the inverse of a matrix
## and to cache the result, so that it can be used for repeated compuation
## of the inverse of the same matrix

## This function serves as cache for a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    Inv <- NULL
    set <- function(y) {
        x <<- y
        Inv <<- NULL
    }
    get <- function() x    
    setInv <- function(I) Inv <<- I
    getInv <- function() Inv
    list (set = set, get = get, setInv = setInv, getInv = getInv)
}


## The function returns the inverse of a matrix and caches the result 
## by using the function makeCacheMatrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    Inv <- x$getInv()
    if(!is.null(Inv)) {
        message("getting cached data")
            return(Inv)
    }
    else {
        data <- x$get()
        Inv <- solve(data)
        x$setInv(Inv)
        Inv
    }
}
