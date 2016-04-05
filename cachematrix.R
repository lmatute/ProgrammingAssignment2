
## returns a list(of functions ) used in cashSolve

makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
        
}


## returns the inverse of  the matrix input to makeCacheMatrix

cacheSolve <- function(x, ...) {
        inv = x$getinv()
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data = x$get()
        inv = solve(data, ...)
        x$setinv(inv)
        return(inv)
}
