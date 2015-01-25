## The following functions are meant to help under computational load 
## Inverting the same matrix multiple times can result in a unneccessary overhead
## makeCachematrix and cacheSolve allow to compute the inverse just once and 
## put it to cache so that it can be associated with the original matrix
## and never be calculated again

## makeCacheMatrix provides tha ability to set and get the value of the matrix
## and of its inverse (note: matrix is assumed to be always invertible)

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL #Set the inverse to NULL so that R does not need to look in higher env for the value
        
        #Setter and getter for the makeCacheMatrix function        
        set <- function(y){
                x<<- y # Set the value of x to y in the parent environment
                inverse <<- NULL #Set the inverse to NULL in the parent environment
        }       
        get <- function() x #Get the value of x
        
        setInverse <- function(solve) inverse<<-solve #Set the value of inverse to solve in the parent environment
        getInverse <- function() inverse #Get the value of inverse
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## cacheSolve calculates the matrix that is the inverse of 'x'. 
## It caches the inverse  after the first call so that it does not need to
## do the computation again but just retrieve the value from cache
cacheSolve <- function(x, ...) {
        
        inverse <- x$getInverse() #Get the value of the inverse from the makeCacheMatrix function 
        
        #Accessing the if only when the inverse is not null, therefore retrieving the cached value
        if(!is.null(inverse)){ 
                message("Getting cached data")                
                return(inverse)
        }
        
        #If the inverse was null it will be calculated and set in the following block
        data <- x$get()
        inverse <- solve(data,...)
        x$setInverse(inverse)
        inverse
}
