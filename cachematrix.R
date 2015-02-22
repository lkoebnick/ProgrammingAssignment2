## The following functions calculate the inverse of a matrix
## and cache it so that it can later be called rather than recalculated.

## The makeCacheMatrix function calculates and stores inverse matrices

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL    ## delete any stored matrix
    set <- function(y) {
        x <<- y
        m <<- NULL    ## delete any stored matrix
    }
    get <- function() x    ## get original matrix 'x'
    setinverse <- function(solve) m <<- solve    ## calculate inverse matrix
    getinverse <- function() m    ## print inverse matrix
    list(set = set,
         get = get,    
         setinverse = setinverse,
         getinverse = getinverse)
}


## The cacheSolve function retrieves stored inverse matrices from cache
## and calculates new ones, which are then cached.

cacheSolve <- function(x = matrix(), ...) {
    ## return a matrix that is the inverse of 'x'
    
    m <- x$getinverse()    ## check if inverse matrix is already stored in cache
    if(!is.null(m)) {    ## inverse matrix is in cache
        message ("getting cached data")
        print(m)    ## print cached inverse matrix
    }
    matrix <- x$get()
    m <- solve(matrix, ...)  ## calculate inverse matrix and
    x$setinverse(m)    ## store in cache
    m    ## print inverse matrix
}

