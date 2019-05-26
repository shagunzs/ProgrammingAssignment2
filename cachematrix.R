## This program has been written for completion of R Programming week 3 assignment.
## This is a 2 step program. In the first part, it calculates an input matrix's inverse.
## and further stores it in the cache memory. In the second part it attempts to directly inverse
## the matrix and arrive at the original matrix.
## It aims to reduce computational speed in the first part, as in multiple iterations of the 
## program, the first part (input remaining same) would remain same.

## This function calculates and then stores in cache memory, the inverse of the input matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse 
        getinv <- function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function is computes the inverse of the "matrix" calculated above.
## If the matrix is not changed (and it was calculated in the cache before),
## then the following function will fetch the solution matrix from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## x is the output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
        
        inv <- x$getinv()
        
        # if the inverse has already been calculated
        
        if (!is.null(inv)){
                # fetches cache solution 
                message("getting cached data")
                return(inv)
        }     
        
        # else, calculates the inverse 
        data <- x$get()
        inv <- solve(data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        
        return(inv)
   }
