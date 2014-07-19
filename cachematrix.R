## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverted matrix
## get the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        invertedMatrix <- NULL
        set <- function(originalMatrix){
                x <<- originalMatrix
                invertedMatrix <<- NULL
        }
        get <- function() x
        setinversion <- function(inversion) invertedMatrix <<- inversion
        getinversion <- function() invertedMatrix
        list(set = set, get = get,
             setinversion = setinversion,
             getinversion = getinversion)
}


## The following function calculates the inverted matrix of the special "vector" created with the above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invertedMatrix <- x$getinversion()
        if(!is.null(invertedMatrix)) {
                message("getting cached data")
                return(invertedMatrix)
        }
        data <- x$get()
        invertedMatrix <- solve(data)
        x$setinversion(invertedMatrix)
        invertedMatrix
}
