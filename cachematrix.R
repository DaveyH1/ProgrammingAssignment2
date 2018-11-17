## The purpose of thes functions is to accept a matrix as an argument
## into the makeCacheMatrix function, determine the inverse of the
## matrix and store it in cache for suture use to avoid the need
## to recomute it. THese functions assumes matrix is invertible

## This function accepts a matrix as an argument x and creates
## constructor methods (get/set) which are used further on in the code
## set will set m to NULL and y to x in the parent environment so they
##      can be accessed by other functions. This is using lexical scoping
## get tell the program where to get the value of m (which will be our 
##      inverted matrix)
## setSolve uses the R solve function in or to invert the matrix and places
##      the final value in the parent environment
## getSolve again tells the program where to get the value of m
## finally this function return a list of the constructors providing a 
## fully instatiated function ready for the next steps

makeCacheMatrix <- function(x = matrix()) {
        m <<- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) m <<- solve
        getSolve <- function() m
        list(set = set, get = get,
             setSolve = setSolve, 
             getSolve = getSolve)
}


## The cacheSolve function will look to see if there is an already
## existing matrix result in the cache using x$getSolve(). If there is
## an existing matrix !is.na(m) returns a TRUE, then it will display the 
## result from cache and exit from the function.
## If however there is not result, it will take the inverted matrix and
## then put it into cache using x$setSolve.
## Finally the inverted matrix is displayed on screen.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getSolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)
        m
}
