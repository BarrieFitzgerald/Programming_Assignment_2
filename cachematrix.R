## This is a pair of functions (makeCacheMatrix and cacheSolve) that cache the inverse of the matrix.

## This function is for the makeCacheMatrix, which will create a special matrix so the inverse
## cache can be performed.
makeCacheMatrix <- function( a = matrix() ) {
        i <- NULL
        setmtrx <- function( matrix ) {
                a <<- matrix
                i <<- NULL
        }
        getmtrx <- function() {
                a
        }
        setmtrxInverse <- function(inverse) {
                i <<- inverse
        }
        getmtrxInverse <- function() {
                i
        }
        list(setmtrx = setmtrx, getmtrx = getmtrx,
             setmtrxInverse = setmtrxInverse,
             getmtrxInverse = getmtrxInverse)
}

## This function is for the cacheSolve. This will compute the inverse of the matrix for makeCacheMatrix
## above. 
cacheSolve <- function(x, ...) {
        a <- x$getmtrxInverse()
        if( !is.null(a) ) {
                message("getting cached data")
                return(a)
        }
        data <- x$getmtrx()
        a <- solve(data) %*% data
        x$setmtrxInverse(a)
        a
}
