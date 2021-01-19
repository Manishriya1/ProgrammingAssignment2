# This in an alternative implementation of a matrix that facilitates costly 
# computation of the inverse of a matrix. Once the inverse is computed it is 
# stored in cache, so there is no need to recompute it. When a matrix changes
# its inverse is recomputed when accessed. The matrix and its inverse are
# stored in the related environment.
    
makeCacheMatrix <- function(x = matrix()) {
    # Creates a special "matrix" object that can cache its inverse.
    #
    # Args:
    #   x: a square matrix
    # Returns:
    #   The list of four functions to access/modify (get/set) the matrix 
    #   and its inverse.
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inv.new) inv <<- inv.new
    getInverse <- function() inv
    return(list(set = set, get = get, 
                setInverse = setInverse, getInverse = getInverse))
}


#cacheSolve: This function computes the inverse of the special "matrix" returned
#by makeCacheMatrix above. If the inverse has already been calculated (and the
#matrix has not changed), then the cachesolve should retrieve the inverse from
#the cache. # Write a short comment describing this function

cacheSolve <- function(x, ...) {
    # Computes the matrix that is the inverse of matrix 'x'. The inverse is 
    # computed only once and stored in cache; when needed later it is retreived 
    # from cache.
    #
    # Args:
    #   x: a special matrix created by function "makeCacheMatrix"
    # Returns:
    #   The inverse of matrix 'x'; the result is a regular matrix
    #
    # Warning: It is assumed that the matrix 'x' is invertible.
    
    # Check if the inverse is in the cache (if it was already computed)
    inv <- x$getInverse()
    if(!is.null(inv)) {
        # No need to recompute the inverse; it was already cached
        return(inv)
    } else {
        # The inverse was not computed yet. Get the raw matrix.
        x.matrix <- x$get()
        # Compute the inverse of matrix 'x' for the first time, cache it and return it
        inv <- solve(x.matrix, ...)
        x$setInverse(inv)
        return(inv)
    }
}

# Example
# a = matrix(c(4,2,7,6), nrow = 2)
## The inverse of a: a.inv = matrix(c(0.6,-0.2,-0.7,0.4), nrow = 2)
# a.cm = makeCacheMatrix(a)
# cacheSolve(a.cm)
# a.cm$get() %*% cacheSolve(a.cm)
