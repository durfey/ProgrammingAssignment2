## These functions do some overall stuff that's useful for
## completing the 2nd programming assignment. More specifically,
## these functions are useful for returning a matrix's inverse by
## first checking to see if the inverse has already been calculated
## and cached. If it has, it simply returns it without recalculating
## it. If it has not been cached yet, it is calculated, cached, and
## returned.

## This function creates a "special matrix" which includes functions
## that sets & stores info about whether or not a cached inverse matrix
## exists & what that inverse matrix is. This info is then used by
## the subsequent function.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## This function first checks to see if the inverse of the inputted
## matrix has already been computed and cached (using information
## created and stored by the previous function. If the inverse
## matrix has been cached, then it simply returns it. If it has not
## been cached, then it calculates it, caches it, and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getInverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}