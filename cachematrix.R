## makCacheMatrix creates a special matrix and cacheSolve provides its inverse
## If the inverse of the matrix has already been calculated it will find it in the cache and return it
## It will not calculate it again.

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to 
## set the value of the Matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse


makeCacheMatrix <- function(x = matrix( )) {
        in_x <- NULL
        set <- function(y) {
                x <<- y
                in_x <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) in_x <<- inverse
        getinverse <- function() in_x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The function cacheSolve returns the inverse of a matrix x created with
## the makeCacheMatrix function.
## If the cached inverse is available, cacheSolve retrieves it without calculating it again
## If not, it computes, caches, and returns it.

cacheSolve<- function(x, ...) {
        in_x <- x$getinverse()
        if(!is.null(in_x)) {
                message("getting cached data")
                return(in_x)
        }
        data <- x$get()
        in_x<- solve(data)
        x$setinverse(in_x)
        in_x
}

## Sample Run
## > x= matrix(1:4,2,2)
## m<- makeCacheMatrix(x)
## > m$get()
##        [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > cacheSolve(m)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(m)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## >

