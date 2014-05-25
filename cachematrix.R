## Put comments here that give an overall description of what your
## functions do

## The function below creates an matrix consisting of the inputted value matrix and has space to cache its inverse once computed
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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    
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
