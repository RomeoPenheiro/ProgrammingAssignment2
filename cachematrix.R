makeCacheMatrix <- function(x = matrix()) {
        ## The assumption is x will be a square invertible matrix
        ## The 'makeCacheMatrix' function will return: a list containing functions to
        ##              A. set the matrix
        ##              B. get the matrix
        ##              C. set the inverse
        ##              D. get the inverse
        ## This list is used as the input to cacheSolve(), the second function
        
        inv <- NULL
        set <- function(y) {
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse 
        getinv <- function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
        ## Here x is the output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
        
        inv = x$getinv()
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
        
        # otherwise, calculates the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        
        return(inv)
}