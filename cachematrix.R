## This function creates a special "matrix" object that can cache its inverse.
## Name makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
        mat1 <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(cacheSolve) mat1 <<- cacheSolve
        getinverse <- function() mat1
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and 
# the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat1 <- x$getinverse()
        if(!is.null(mat1)) {
                message("getting cached data")
                return(mat1)
        }
        data <- x$get()
        mat1 <- solve(data) %*% data
        x$setinv(mat1)
        mat1
}
