## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Thus function input a invertiable matrix, and 
## provides a list of functions to get and set the already computed inverse
## of this matrix. If the inverse is not computed, it would 
## take on a NULL value. 
## It also provides a set function of set a new matrix. 

makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL
    set <- function(y) {
        x <<- y
        x_inv <<- NULL
    }
    get <- function() x
    setInv <- function(inv) x_inv <<- inv
    getInv <- function() x_inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Write a short comment describing this function 
## This function takes in the list of functions returned by 
## makeCacheMatrix, and returns the inverse of the matrix
## stored in the object created by makeCacheMatrix. 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    x_inv <- x$getInv()
    if(!is.null(x_inv)) {
        message("getting cached data")
        return(x_inv)
    }
    data <- x$get()
    x_inv <- solve(data, ...)
    x$setInv(x_inv)
    x_inv
}
