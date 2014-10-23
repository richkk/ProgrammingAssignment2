## This file contains a pair of functions that cache the inverse of a matrix. Matrix inversion
## is usually a costly computation and thus there is a benefit to caching the inverse of a matrix 
## rather than compute it repeatedly.
## 
##  Usage:
##    For a given square matrix "m" do the following:
##     > cachedM <- makeCacheMatrix(m)  
##     > cacheSolve(cachedM)
##    cachedM now contains the cached inverse of "m" and can be retrieved:
##     > cachedM$getInverse()


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    im <- NULL # the inverse matrix. Default to null.
    
    #this inner function allows user to change the original matrix
    set <- function(y) {
        x <<- y     #set new matrix
        im <<- NULL #null the inverse matrix 
    }
    
    # return the original matrix
    get <- function() {x}
    
    # set the inverse matrix in the cache
    setInverse <- function(inverse) { im <<- inverse }
    
    # return the inverse matrix from the cache (may be NULL)
    getInverse <- function() {im}
    
    #return the list of functions 
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve 
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Get the inverse from the cache
    im <- x$getInverse()
    ## if in cache then just return it
    if (!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    ## else get the matrix, validate it is not null and is square and then get the inverse
    ## via solve function and save it in the cahce.
    m <- x$get()
    if (!is.null(m)) {
        if (nrow(m)==ncol(m)) {
            im <- solve(m)
            x$setInverse(im) 
        } else{
            warning("Not a square matrix!")
        }            
    }  
    
    #return the inverse of the matrix (may be null)
    im        
}
