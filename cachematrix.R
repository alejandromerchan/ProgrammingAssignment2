## I'm not pretending to be original with this homework. While I did not copied 
## anything from the web, I clearly just took the example from the class and
## replaced some of the names and the "mean" function for the "solve" function.


## This function creates a list of functions that manipulate the information
## provided and create what are called "getter" and "setter" functions in
## https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md

makeCacheMatrix <- function(x = matrix()) {
        # Assign a Null value to he object Inv
        Inv <- NULL
        # "setter" function that assigns values at the parent environment
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        # getter function to retrieve x from the parent environment 
        get <- function() x
        #  setter function defines the inverse
        setinv <- function(inv) Inv <<- inv
        # getter for the inverse
        getinv <- function() Inv
        # Returns a list
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function takes an object from funtion makeCacheMatrix and applies the 
## different function in the list. If the information from Inv is in cache 
## memory, the function retrieves it, otherwise it calculates the new inverse 
## matrix

cacheSolve <- function(x, ...) {
        # Assign the value from the getinv function to Inv
        Inv <- x$getinv()
        # This if loop checks for the value of Inv in the parent environment.
        # If present, returns that value
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        # assigns the provided matrix to data
        data <- x$get()
        # This uses the solve function to take the matrix and calculate the 
        # inverse, assigning it to the object Inv
        Inv <- solve(data, ...)
        # Sets the value of Inv 
        x$setinv(Inv)
        # Calls the value of Inv
        Inv
}