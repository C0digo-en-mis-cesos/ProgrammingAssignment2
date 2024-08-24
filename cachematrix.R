##These functions create a matrix object and store the computation of its inverse.

##(1) This function creates a special "matrix" object that can cache its inverse. This function creates a vector/list containing a function to:

##set the value of the matrix

##get the value of the matrix

##set the value of the inverse of the matrix

##get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    ##erasing any value that s may have had elsewhere in the environment so we can reassign it here
    s <- NULL     
    ##saying what we want our makeCacheMatrix() function to do
    set <- function(y) {
        ##This means that what happens to y is actually what is done to x, our initial input value (matrix)
        x <<- y
        ##erasing any value that s may have had elsewhere in the environment so we can reassign it here (again?)
        s <<- NULL
    }
    ##Calling the function we made, inputting x
    get <- function() x
    ##Setting the value of the inverse of the matrix inputted
    setinverse <- function(solve) s <<- solve
    ##Calling/printing the inverse of the matrix inputted as a list
    getinverse <- function() s
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## (2) This function computes the inverse of the matrix value, drawing on the cached inverse value. If it can't find that cached value, it creates the matrix itself given x/the input.

##Making next function; the "..." signifies other arguments you don't need to list. 
cacheSolve <- function(x, ...) {
   ##assigning cached value for inverse of matrix
    s <- x$getinverse()
    ##If it CAN find the cached value, print that and this message
    if(!is.null(s)) {
        message("getting cached data")
        ##print that cached value
        return(s)
    }
    ##If it CAN'T find the cached value, input x into the makeCacheMatrix function
    data <- x$get()
    ##solve/find the inverse of that matrix
    s <- solve(data, ...)
    ##Set that value as the inverse
    x$setinverse(s)
    #Print/return the inverse value
    return(s)
}
