## The first function sets up the parent environment, which will store
## the cache memory inverse matrix.  It also creates a list whereby
## the cacheSolve function can query inorder to determine if the inverse 
## has been created.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) m <<- inverse
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## The second function inputs a matrix and creates its inverse; however, 
## prior to running the inverse function, it looks in cache memory to see
## if the inverse matrix has already been created.  If it has then the function
## returns the inverse matrix from cache memory and a message that the data
## is from cache memory.  Otherwise, the function
## solves for the inverse matrix and store it in cache memory.


cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

## Sample
## x<- matrix(1:4, 2,2)
## m$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
##
## The first time cacheSolve is run, nothing is in cache memory
## so the solve function is executed and we do not get a message 
## "getting cache data.
##
## cacheSolve(m)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## The second time cacheSolve is run, the inverse matrix has been stored in 
## cache memory and we get the message that "getting cached data"
##
## cacheSolve(m)
## getting cached data
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

