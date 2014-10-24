## This is a pair of functions (makeCacheMatrix and cacheSolve) that cache the inverse of the matrix.

## This function is for the makeCacheMatrix, which will create a special matrix so the inverse
## cache can be performed.
makeCacheMatrix <- function( a = matrix() ) {
        ## Creating the inverse
        i <- NULL
        ## Setting and Getting the matrix that will be used
        setmtrx <- function( matrix ) {
                a <<- matrix
                i <<- NULL
        }
        getmtrx <- function() {
                a
        }
        ## Setting and Getting the matrix's inverse
        setmtrxInverse <- function(inverse) {
                i <<- inverse
        }
        getmtrxInverse <- function() {
                i
        }
        ## Prints a list cotaining the actions
        list(setmtrx = setmtrx, getmtrx = getmtrx,
             setmtrxInverse = setmtrxInverse,
             getmtrxInverse = getmtrxInverse)
}

## This function is for the cacheSolve. This will compute the inverse of the matrix for makeCacheMatrix
## above. 
cacheSolve <- function(x, ...) {
        ## The inverse of the matrix is returned
        a <- x$getmtrxInverse()
        ## This will return the inverse if already calculated
        if( !is.null(a) ) {
                message("getting cached data")
                return(a)
        }
        ## Gets and calculates the inverse matrix
        data <- x$getmtrx()
        a <- solve(data) %*% data
        x$setmtrxInverse(a)
        ## Prints the matrix
        a
}
