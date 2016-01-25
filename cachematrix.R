## Put comments here that give an overall description of what your
## functions do
#makeCachMatrix creates a special matrix object that can cache its inverse
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
mat <- NULL
        set <- function(y) {
                x <<- y
                mat <<- NULL
        }
        get <- function() x
        set_inverse <- function(solve) mat <<- solve
        get_inverse <- function() mat
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}


## Write a short comment describing this function
# cacheSolve computes the inverse of the special matrix returned by makeCachMatrix. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat <- x$get_inverse()
        if(!is.null(mat)) {
                message("retrieving cached data")
                return(mat)
        }
        data <- x$get()
        mat <- solve(data, ...)
        x$set_inverse(mat)
        mat
}
