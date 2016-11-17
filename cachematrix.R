## The two functions below are used to create a special matrix and cache its inverse

## Function makeCacheMartix creates a matrix, a special one that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- inverse 
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)        
        
}


## The function cacheSolve returns the inverse of a given special matrix from function 
## makeCacheMatrix (we assume here that the matrix is inversible). If the inverse has 
## already been calculated and exists in cache, it will be retrieved from there, if not, 
## it will calculate the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

