##These functions create a matrix, store its value, then compute its inverse.

## Write a short comment describing this function

##(1) This function creates a vector/list containing a function to

set the value of the vector

get the value of the vector

set the value of the matrix

get the value of the matrix

makeCacheMatrix <- function(x = numeric()) {
    m <- NULL                                         
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(matrix) m <<- matrix
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## (2) This function computes the inverse of the cached matrix value. If it can't find that cached value, it creates the matrix itself given x/the input.

cacheSolve <- function(x, ...) {
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- matrix(data, ...)
    x$setmatrix(m)
    m
    solve(m)
}
