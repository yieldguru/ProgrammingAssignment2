## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix and cacheSolve take in a square matrix as a user input and inverts it.  If the matrix has not
## been inverted then the cacheSolve function will invert (solve()) the matrix.  If the matrix has already been inverted,
## the inverse will be pulled from cache instead of recalculating.
# This first function, makeCacheMatrix, creates an object that stores a matrix and its inverse.  The second function,
# cacheSolve, requires an output object from the first function (i.e. myMatrix <- makeCacheMatrix(matrix(definematrixhere))).  In that object are arguments 
# needed to get the cached value that is stored in the makeCacheMatrix environment.

### Write a short comment describing this function

# makeCacheMatrix takes a matrix (predefined as being invertible-square) and stores it to a variable x.  It creates 4 functions
# and returns them to the parent environment, via the "<<-" assignment.  These functions can be called after giving makeCacheMatrix an input matrix
# and outputting to an object (i.e. mymatrix$get, mymatrix$set, mymatrix$getinverse, mymatrix$setinverse).


makeCacheMatrix <- function(x = matrix()) {
    i <- NULL                  #  when makeCacheMatrix is populated with a matrix, i is set to null as there is no cached value.  Needed to make sure it doesn't end up as an undefined.
    set <- function(y) {       #  y is the the input matrix x
        x <<- y                #  y is populated from X value retrieved from parent environment
        i <<- NULL             #  clears any cached value of i from previous run of cacheSolve.  whenever set is run the cache is cleared so the correct inverse is calculated.
    }
    get <- function() x        #  allows user to see what's in output object (i.e myMatrix$get)
    setinverse <- function(inverse) i <<- inverse  # since i would normally go away after setinverse was run, the way to make it stay is to put it into the parent environment wiht <<-
    getinverse <- function() i 
    
    list(set = set, get = get, #  allows user to access functions 
         setinverse=setinverse,getinverse=getinverse
    )
    
}    

## Write a short comment describing this function
## cacheSolve takes the output object from makeCacheMatrix (i.e. myMatrix) as input.  It also includes all the information about the environment the variables were created in.
## checks to see if it is null.  If so, calculates the value with solve()
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## browser()
    i <- x$getinverse()        #Pulls in the inverse from the makeCacheMatrix.  
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...) #where the actual inverse is calculated
    x$setinverse(i) #runs setinverse funtion with the input object (i.e. myMatrix)
    i
}


