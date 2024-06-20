## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the inverse to NULL
    
    ## Set the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL  # Reset the inverse cache when the matrix is changed
    }
    
    ## Get the value of the matrix
    get <- function() x
    
    ## Set the value of the inverse
    setinverse <- function(inverse) inv <<- inverse
    
    ## Get the value of the inverse
    getinverse <- function() inv
    
    ## Return a list of the functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed,
## then cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Check if the inverse has already been calculated
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)  # Return the cached inverse
    }
    
    ## Compute the inverse
    mat <- x$get()
    inv <- solve(mat, ...)  # Use solve() to compute the inverse
    x$setinverse(inv)  # Cache the inverse for future use
    inv  # Return the inverse
}