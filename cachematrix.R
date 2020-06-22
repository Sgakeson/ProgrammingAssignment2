## Function #1 makeCacheMatrix takes a user input matrix and contains
## subfunctions capable of calling up and editing cached relevant information 
## about the matrix.
## Function #2 cacheSolve will check to see if a given matrix has a cached
## inverse and if not will calculate it.


## makeCacheMatrix:
## This function takes a user input matrix created through the
## matrix function and upon call sets matrix_inverse to Null. This makes sure
## every time makeCacheMatrix is called there wont be any leftover results.
## This function returns a list of all of the functions within makeCacheMatrix.
makeCacheMatrix <- function(x = matrix()) {
        matrix_inverse <- NULL
        set <- function(y) {
                x <<- y
                matrix_inverse <<- NULL
        }
        get <- function() {x}
        setinverse <- function(inverse) {matrix_inverse <<- inverse}
        getinverse <- function() {matrix_inverse}
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve:
## This function begins by accessing the cached inverse from the makeCacheMatrix
## function. It proceeds to test this value and if it is Null cacheSolve will go
## on to calculate and save the inverse value of the matrix. This function
## returns a matrix that is the inverse of 'x'.

cacheSolve <- function(x, ...) {
        matrix_inverse <- x$getinverse()
        if(!is.null(matrix_inverse)){
                print("Getting cached data: ")
                return(matrix_inverse)
        }
        input <- x$get()
        matrix_inverse <- solve(input, ...)
        x$setinverse(matrix_inverse)
        matrix_inverse
        ## Return a matrix that is the inverse of 'x'
}
