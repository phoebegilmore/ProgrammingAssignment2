## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setIM <- function(solvedm) im <<- solvedm
    getIM <- function() im
    list(set = set, get = get,
         setIM = setIM,
         getIM = getIM)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    im <- x$getIM()
    if(!is.null(im)) {
        message("getting cached inversed matrix")
        return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setIM(im)
    im
}
