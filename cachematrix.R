## Programming Assignment 2 for R Programming on Coursera
## Assignment: Caching the Inverse of a Matrix


## 1. Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function( x = matrix() ) {

	## Initialize the inverse property
    m <- NULL

    ## Use the set method to set the value of the matrix
    set <- function( y ) {
            x <<- y
            m <<- NULL
    }

    ## Use the get method the get the value of the matrix
    get <- function() x

    ## Set the inverse of the matrix & store in the cache
    setInverse <- function(inverse) m <<- inverse
    

    ## Get the inverse of the matrix from the cache
    getInverse <- function() m
    

    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## 2. Compute the inverse of the special matrix returned by "makeCacheMatrix"
##    above. If the inverse has already been calculated (and the matrix has not
##    changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

    ## Get the matrix that is the inverse of 'x' from the cache
    m <- x$getInverse()

    ## If it's already exists return the inverted matrix
    ## Else create the matrix in working environment
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get the matrix from our object
    data <- x$get()

    ## Calculate the inverted matrix with solve() function
    m <- solve(data, ...)

    ## Set the inverted matrix in the cache
    x$setInverse(m)

    ##Display the matrix in the console
	return(m)
}
