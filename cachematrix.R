## Function 'makeCacheMatrix' kinda keeps an input matrix and defines nested functions
## Function 'cacheSolve' manipulates the matrix to find and store its inverse

## Function 'makeCacheMatrix' kinda keeps an input matrix and defines nested functions 
## Its nested functions include resetting a new matrix, passing matrix to a calling function, 
## storing the inverse of the matrix, and passing inverse to a calling function

makeCacheMatrix <- function(x = matrix()) {
        inv1 <- NULL
        set <- function(y) {
                x <<- y
                inv1 <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv1 <<- inverse
        getinverse <- function() inv1
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Function 'cacheSolve' manipulates the matrix to find and store its inverse
## It first checks whether inverse of matrix in 'makeCacheMatrix' has already been calculated.
## If not, it reads the matrix from 'makeCacheMatrix', calculate the inverse, then pass it back

cacheSolve <- function(x, ...) {
        inv2 <- x$getinverse()
        if(!is.null(inv2)) {
                message("getting cached data")
                return(inv2)
        }
        data <- x$get()
        inv2 <- solve(data)
        x$setinverse(inv2)
        inv2
}
