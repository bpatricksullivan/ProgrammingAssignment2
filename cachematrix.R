makeCacheMatrix <- function(x = matrix()) {
        #This function creates a matrix object (a named list, actually) that is 
        # able to cache its  own inverse. For this assignment, we're assuming 
        # the matrix is square and the inverse exists (x is non-singular).
        # The elements of the named list will be:
        # set, get, setInverse, and getInverse
        
        inv <- NULL #initialize inv
        
        set <- function(y){ #define the function "set" to take argument y.
                x <<- y #assign y to x in the parent environment 
                inv <<- NULL #assign Null to inv in the parent environment
        }
        
        get <- function() {x}
        setInverse <- function(inverse) {inv <<- inverse}
        
        getInverse <- function() {inv}
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
        ## This function computes the inverse of the special “matrix”, x, 
        ## returned by the function, makeCacheMatrix, above. If the inverse of x
        ## has already been calculated, then cacheSolve 
        ## should retrieve the inverse from the cached value.
        inv <- x$getInverse()
        if(!is.null(inv)){ #if the cached inverse exits...
                message("Retrieving cached inverse...") #tell me so
                return(inv) #and return it. #return will break here.
        }
        #otherwise:
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv #the function automatically returns the result of its last line
}
