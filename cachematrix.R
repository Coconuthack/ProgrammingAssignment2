## The two functions below  make matrix computations more efficient by
## providing a way to create a matrix, compute its inverse and cache it for 
## future computation 

## This function creates a special matrix with space to set and get its cached 
## inverse once computed
makeCacheMatrix <- function(x = matrix(), ...) {
    i <- NULL
    
    ## to be able to make matrix with initialized arguments
    ## e.g. m <- makeCacheMatrix(1,2,2) which is a 2x2 matrix with 1 for each elemen
    x <- matrix(x, ...) 
    
    set <- function(y, ...){
        ## replaces the intialized/old matrix based on 'y, ...' values
        x <<- matrix(y, ...) 
        i <<- NULL ## resets the inverse
    }
    
    get <- function() x #returns the cached matrix
    
    ## sets the cached inverted matrix to the supplied 'inverse' value
    setInverse <- function(inverse) i <<- inverse
    
    getInverse <- function() i
    
    ## returns a list with the values as the set and get functions
    ## and makes the functions accessible via subsetting e.g. x$set(matrix) 
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
    
    ## assignes the matrix, that is stored in the special matrix object, to 'matrix'
    matrix <- x$get()
    
    ## solves teh inverse of the retrieved matrix
    i <- solve(matrix, ...)
    
    ## stores the solved inverse of the matrix in the special matrix object
    x$setInverse(i)
    
    ## Return a matrix that is the inverse of 'x'
    i
}
