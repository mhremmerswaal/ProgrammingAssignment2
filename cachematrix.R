## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        #Function to create matrix in cache
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(Inverse) m <<- Inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #check if the inverse existst
        m <- x$getInverse()
        if(!is.null(m)) {
                #When cached version exists, retreive cached matrix and exist function
                message("getting cached data")
                return(m)
        }
        #If the cached function does not exists the inverse matrix is beiging determined and stored in cache
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m        
}

#Test example
TestMatrix <- matrix(1:4,2,2)
TestMatrix_Cache <- makeCacheMatrix(TestMatrix)

cacheSolve(TestMatrix_Cache)
