## Put comments here that give an overall description of what your
## functions do

## this function creates a special matrix (square matrix)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL        ## initializing inverse as NULL
        set <- function(y) {     ## a function to set the value of the matrix
                x <<- y
                inv <<- NULL
        }
        get <- function() x       ## a function t get the value of the matrix
        setInverse <- function(inverse) inv <<- inverse  # a function to set the inverse of matrix
        getInverse <- function() inv        ## a function to get the inverse of matrix
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## this function calculates the inverse of a matrix created in the above function

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {  # checks if the inverse has already been calculated.
                message("getting cached inverse matrix")
                return(inv)  ## if calculated in returns the cached value
        }
        data <- x$get()     ## it calculates the inverse if not cached already
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv        ## Return a matrix that is the inverse of 'x'
}
