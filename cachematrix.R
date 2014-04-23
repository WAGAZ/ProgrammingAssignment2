
##  The two functions below can be used to solve and/or cache the solution for 
##  providing the inverse of a matrix.  that are used to create a special object that stores a numeric vector and caches its mean.
 


## This function uses the << symbol to store the solution for the matrix inverse within a different 
## environment (function) for future reference.  If called upon by the cacheSolve function and the 
## matrix inverse solution is available, it is provided, thus saving time.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The following function calculates the solution for solving the inverse of a matrix.  What it
## does first is to see if the solution has been already completed and cached in the above function.


cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

> g <- makeCacheMatrix(matrix1)
> cacheSolve(g)
[,1] [,2] [,3]
[1,]    7   -3   -3
[2,]   -1    1    0
[3,]   -1    0    1
> cacheSolve(g)
getting cached data
[,1] [,2] [,3]
[1,]    7   -3   -3
[2,]   -1    1    0
[3,]   -1    0    1
> 