## Creates a special matrix object that can cache its inverse
## A list of functions that set and get the matrix and then
## set and get the inverse of it.

makeCacheMatrix <- function(x = matrix()) {
    
    
    
    ## Initialize the inverse property
    i <- NULL
    
    ## function to set the matrix
    set <- function( matrix ) {
        m <<- matrix
        i <<- NULL
    }
    
    ## function to get the matrix
    get <- function( ) m
    
    
    ## function to set the inverse of the matrix
    setInverse <- function(inverse)   i <<- inverse
    
    
    ## function to get the inverse of the matrix
    getInverse <- function( ) i
    
    ## Return a list of the functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}





## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    
    ## Return the inverse if its already set
    if( !is.null(m) ) {
        message("getting cached data")
        return(m)
    }
    
    ## Get the matrix 
    data <- x$get()
    
    ## Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data
    
    ## Set the inverse to the matrix
    x$setInverse(m)
    
    ## Return the inverse of "x"
    m
}
