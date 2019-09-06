## Together the following two functions enable the quick and easy computation of
## a matrix's inverse.

## the makeCacheMatrix function creates a list of named functions to be applied 
## to a supplied matrix which will enable the calculation of the matrix's inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<-NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## When the result of the makeCacheMatrix function (x) is fed into cacheSolve, the 
## cacheSolve function calls getinverse which pulls the most recent value for inv.
## If an inverse has previously been calculated, the value is the inverse of the 
## original object, which sends the function through the if{} statement and the inverse
## is then returned and displayed. If an inverse for that matrix object 
## has not been calculated, the value of m becomes NULL and the function skips
## the if{} statement and proceeds to call in the matrix object and calculate its
## inverse.
#

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

# Example usage:
#
# > source("U:/R/GITHUB/ProgrammingAssignment2/cachematrix.R")
# > x <- matrix(5:8,2,2)
# > y <- matrix(15:18,2,2)
# > z <- matrix(25:28,2,2)
# > x.makecache <- makeCacheMatrix(x)
# > y.makecache <- makeCacheMatrix(y)
# > z.makecache <- makeCacheMatrix(z)
# > cacheSolve(x.makecache)
# [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5
# > cacheSolve(y.makecache)
# [,1] [,2]
# [1,]   -9  8.5
# [2,]    8 -7.5
# > cacheSolve(x.makecache)
# getting cached data
# [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5
# > cacheSolve(z.makecache)
# [,1]  [,2]
# [1,]  -14  13.5
# [2,]   13 -12.5
# > cacheSolve(y.makecache)
# getting cached data
# [,1] [,2]
# [1,]   -9  8.5
# [2,]    8 -7.5
