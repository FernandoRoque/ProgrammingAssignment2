## Programm that calculates the inverse of a matrix
## If this iverse exists, it only shows the results

## This function create a special matrix-list

makeCacheMatrix <- function(x = matrix()) {

        s   <- NULL
        set <- function(y)
           {x <<- y
            s <<- NULL}
        get <- function() x

        setsolve <- function(solve) s <<- solve
        getsolve <- function() s

        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function retur the inverse of the matrix
## If thisthe inverse exists, only retur that value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }

        data <- x$get()
        s    <- solve(data)
        x$setsolve(s)
        s
}
