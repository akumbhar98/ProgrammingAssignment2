makeCacheMatrix <- function(x=matrix())
{
        iv <- NULL
        set <- function(z)
        {
                x <<- z
                iv <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) {iv <<- inverse}
        getInverse <- function() {iv}
        list(set = set, get = get, setInverse = setInverse, getInverse=getInverse)
}

cacheSolve <- function(x, ...)
{
        v <- x$getInverse()
        if(!is.null(iv))
        {
                message("getting cache data")
                return(iv)
        }
        mat <-x$get()
        v <- solve(mat,...)
        x$setInverse(iv)
        iv
}
 