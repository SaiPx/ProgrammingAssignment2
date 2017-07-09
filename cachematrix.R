## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix is a generic function that can cache large
## previously computed values. This is possible by the "<<-"
## operator and the scope of the variable in the parent Environment

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        ## Think,it does not require to change a Cache Setter/Getter functions
        ## Hence, left As - IS from Sample code
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        
        ## Set and Get Cache Values (In this case it is Inverse, 
        ##                           in Sample code it was mean)
        setinverse <- function(inv) m <<- inv
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## This is the main Inverse function that calls "solve"
## Once the inverse is computed, no matter how many times
## the CacheSolve is called it will return 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        ## Return the cache from second execution onwards
        ## First time this "if" will be passed over to execute rest of code
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
