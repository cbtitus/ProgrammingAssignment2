##Week 3 Programming Assignment, Brian Titus 
##
## makeCacheMatrix is a function that returns a list of functions for our matrix inverter.
## cacheSolve takes the list as an argument and checks to see if an inverse has been stored.
## If it has, then cacheSolve returns the stored value. If it hasn't, it inverts using solve()
##
## makeCacheMatrix function usage:
## To use it to store a 5x5 matrix that we can try inverting, do the following:
## first, create a list: 
## mymatfunc<-makeCacheMatrix()
##
## then, use the list to set up and store a new matrix:
## mymatfunc$set(matrix(rnorm(25),nrow=5,ncol=5))
## we can check to see what was created by typing:
## mymatfunc$get()

makeCacheMatrix <- function(x = matrix()) {
                inv <- NULL
                set <- function(y) {
                        x <<- y
                        inv <<- NULL
                }
                get <- function() x
                setinverse <- function(inverse) inv <<- inverse
                getinverse <- function() inv
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
}


## Once we have a matrix stored, we can use cacheSolve
## The first time we try cacheSolve(mymatfunc), there shouldn't be anything stored 
## for the inverted matrix, so it will get the matrix, and run 'solve' on it. The inverted matrix
## will be returned, and also stored for next time.
## The next time we try cacheSolve(mymatfunc), we should retrieve the stored value instead
## of calculating it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mymatrix <- x$get()
        inv <- solve(mymatrix, ...)
        x$setinverse(inv)
        inv
}
