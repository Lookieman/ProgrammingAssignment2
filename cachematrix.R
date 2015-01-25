## The r script calculates the inverse of the Matrix provided by the user. The function checks first if the inverse has been calculated previously and returns the value,
## else it calculates the inverse and returns the value. The script is made up of two functions, makeCacheMatrix and cacheSolve.

## makeCacheMatrix returns the value of the inverse Matrix. it takes a Matrix as input

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve returns the inverse of the Matrix. It takes the function makeCacheMatrix as an input. 
## The function checks whether the inverse matrix has been calculated previously and returns the value, else calculates the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m

}
