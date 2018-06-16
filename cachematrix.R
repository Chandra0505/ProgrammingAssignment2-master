makeCacheMatrix <- function( m = matrix() ) {
    i <- NULL
    
    # method to set the matrix
    set <- function( matrix ) {
        m <<- matrix
        i <<- NULL
    }
    
    # method the get the matrix
    get <- function() {
        m
    }
    
    #  set the inverse 
    setInverse <- function(inverse) {
        i <<- inverse
    }
    
    ## method to get the inverse of the matrix
    getInverse <- function() {
        i
    }
    
    # return the matrix 
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


# calculate the inverse of the matrix returned by the makeCacheMatrix function above.
# If the inverse has already been calculated , then the function cachesolve
# should retrieve the inverse from the cache.



cacheSolve <- function(x, ...) {
    
    # return matrix that is the inverse of x
    m <- x$getInverse()
    
    # now return the inverse if its already set
    if( !is.null(m) ) {
        message("getting cached data")
        return(m)
    }
    
    # it gets matrix from our object
    data <- x$get()
    
    # calculate the inverse using matrix multiplication
    m <- solve(data) %*% data
    
    
    x$setInverse(m)
    
    m
}