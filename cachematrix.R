## These functions do some overall stuff that's useful for
## completing the 2nd programming assignment

## This function creates a special matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## This function first checks to see if the inverse of the inputted
## matrix has already been computed and cached. If it has, then it 
## returns the cached inverse matrix. If it has not been computed
## yet, then it computes it and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinv()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
