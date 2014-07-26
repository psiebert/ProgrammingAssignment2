##Programming Assigment 2 of the R-Programming Course on Coursera.org
##User: Pablo Siebert

## Explanation of the function that inverse a matrix
## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {

    i <- NULL

## Here we set and get matrix and then we show the matrix.
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL}

    get <- function() {
    	m}

 ## The method to inverse the matrix 
    seteoInversa <- function(inversa)
 {
        i <<- inversa
    }

    tenerInversa <- function() {
        i
    }

    ## Return a list of the methods
    list(set = set, get = get,
         seteoInversa = seteoInversa,
         tenerInversa = tenerInversa)
}


cacheSolve <- function(x, ...) {

    ## Return the inverse of x
    m <- x$seteoInversa()

    if( !is.null(m) ) {
            return(m)
    }

    ## Getting the matrix
    data <- x$get()

    m <- solve(data) %*% data

    ## Set the inverse
    x$seteoInversa(m)

    ## Return m
    m
}
