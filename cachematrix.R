## The function pairs produce matrix inverse
## and store it for future retrival
## Example of usage: 
## Find the inverse of 
## > matrix(c(1,1,-1,1), 2, 2)
##      [,1] [,2]
## [1,]    1   -1
## [2,]    1    1
## > mat_obj <- makeCacheMatrix(matrix(c(1,1,-1,1), 2, 2))
## > cacheSolve(mat_obj)
## [,1] [,2]
## [1,]  0.5  0.5
## [2,] -0.5  0.5
## > cacheSolve(mat_obj)
## getting cached data
## [,1] [,2]
## [1,]  0.5  0.5
## [2,] -0.5  0.5
## >

## This function input is a (square invertable) matrix x
## It stores the matrix inverse in m
## It provides 4 functions to operate on x & m:
## set - Set the original matrix in the main function to input y 
##       & matrix inverse m to NULL
## get - Get the matrix x
## setinverse - Set the matrix inverse to input inverse
## getinverse - Get the stored matrix inverse m

makeCacheMatrix <- function(x = matrix()) {
    ## 
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function returns inverse of the input matrix object x
## If the inverse was computed before, just return it
## Otherwise, it computes the matrix inverse, using solve()
## and stores it in m.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
