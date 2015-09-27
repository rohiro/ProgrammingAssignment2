# Create a cache marix to sove the inverse of a matrix but calculates the inverse once if needed. 
#
# how to use:
#  exampleMatrix <- matrix(c(1:3,4:6, 7:9), nrow=3, ncol=3)
#  cacheMatrix <- makeCacheMatrix(exampleMatrix)
#  cacheSolve(cacheMatrix)
#
#  cacheMatrix$set(exampleMatrix)      # Change the matrix being cached.
#  exampleMatrix <- cacheMatrix$get()  # Returns the matrix being cached.
#
#  cacheMatrix$setInverse(solve(data, ...)) # function containing the cached inverse of input x
#  cacheMatrix$getInverse()                 # function used to obtain the cached inverse of x

# Create the cacheMatrix for the invertable matrix.

makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL
    set <- function(y) {
        x <<- y
        cachedInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) cachedInverse <<- inverse
    getInverse <- function() cachedInverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


# Return the inverse of a cacheMatrix object

cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    MatrixInverse <- x$getInverse()
    if(!is.null(MatrixInverse)) {
        message("getting cached data")
        return(MatrixInverse)
    }
    data <- x$get()
    MatrixInverse <- solve(data, ...)
    x$setInverse(MatrixInverse)
    MatrixInverse 
}