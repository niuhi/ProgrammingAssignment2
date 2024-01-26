## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) { # take new matrix as an input
        x <<- y
        inv <<- NULL
    }
    get <- function() x # returns matrix stored in "matrix cache object"
    setinv <- function(inverse) inv <<- inverse # store inverse matrix in cache
    getinv <- function() inv # returns inverse matrix stored in "matrix cache object"
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # takes cached matrix and tries to retrieve cached inverse matrix
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

# test
#matice <- makeCacheMatrix(matrix(c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, -11, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, -11, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, -11, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, -11, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, -11, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, -11, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, -11, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, -11, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, -11, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, -11, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, -11, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, -11, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, -11, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, -11, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, -11, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, -11, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, -11), nrow = 16))
#cacheSolve(matice)
