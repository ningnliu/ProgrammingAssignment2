## this file contain two functions. The first cache the matrix and
## it's inverse. The second check whether the inverse is in the
## cache. If there is not, then calculate it and store it in## the cache.
## this function cache the matrix and the inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL  }
        get <- function() x  
        setinverse <- function(inverse) inv <<- inverse  
        getinverse <- function() inv  
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)}

## This function check the cache, and calculate the inverse if 
## there is not a inverse of matrix in the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()  if(!is.null(inv)) {
                message("getting cached data")    return(inv)
        }
        data <- x$get()  
        inv <- solve(data,...)  
        x$setinverse(inv)  
        inv
}
