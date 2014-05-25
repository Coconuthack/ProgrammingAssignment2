## The two functions below  make matrix computations more efficient by
## providing a way to create a matrix, compute its inverse and cache it for 
## future computation 

## This function creates a special matrix with space to set and get its cached 
## inverse once computed
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    ## sets the matrix inverse to the supplied 'inverse' value
    setInverse <- function(inverse) i <<- inverse 

    getInverse <- function() i
    list( set = set,
          get = get,
          setInverse = setInverse,
          getInverse = getInverse)   
}


## This function solves the inverse of the special matrix and caches it in the original object, 
## and returns the cached matrix
cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    
    ## returns the cached inverse if already compute ( and exits the function )
    if(!is.null(i)){
        message("Retrieving cached data")
        return(i)
    }
    
    matrix <- x$get
    i <- solve(x, ...)
    ## stores the solved inverse matrix to the special matrix object
    x$setInverse(i)
    ## Return a matrix that is the inverse of 'x'
    i
}
