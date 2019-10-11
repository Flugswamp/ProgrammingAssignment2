## makeCacheMatrix makes a list object that contains functions and matrices
## cacheSolve calculates the inverse of a makeCacheMatrix unless the makeCacheMatrix
## object already contains a stored inverse

## makeCacheMatrix takes a matrix as argument and creates a list of 4 
## different functions to perform operations on this matrix as well as 
## store the matrix and it's inverse if available.

makeCacheMatrix <- function(x = matrix()) {
        
        ## Initialize an empty object so that it can be used directly without using set
        m <- NULL 
        
        ## Create list that holds functions to manipulate the matrix
        list ( 
                
                ## Set a new value for the matrix in the enclosing environment and reset m        
                set = function(y) {
                        x <<- y     
                        m <<- NULL  
                },
                ## Return the current value of the matrix
                get = function() x, 
                ## Assign the inverse value (passed from CacheSolve)
                setinverse = function(inverse) m <<- inverse, 
                ## Return the current value of the inverse
                getinverse = function() m 
        )
        
}

## Takes a makeCacheMatrix object as input and computes the inverse unless 
## the makeCacheMatrix environmment already contains an inverse (m is not null)

cacheSolve <- function(x = list(), ...) {
        ## Get current value of m from the makeCacheMatrix object
        m <- x$getinverse() 
        
        ## Check if null and if not null return m and exit
        if(!is.null(m)) {                       
                message("getting cached data")
                return(m)
        }
        
        ## Otherwise assign the current value of the matrix to data and calculate inverse
        data <- x$get()
        m <- solve(data, ...)
        ## Store inverse in the cache
        x$setinverse(m)
        
        ## Print inverse
        m
}
