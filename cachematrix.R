## Two functions which cache inversed matrices and either give back cached matrix
## of previously inversed matrix, or does inversion of matrix via "solve"

## This function creates a specific matrix format to be given to cacheSolve
## in order to calculate it's inversed matrix or retrieves calculated inversed
## matrix from cache

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


## This function checks if x has already been inversed previously
## if yes it retrieves cached inversed matrix, otherwise it calculates it new

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
