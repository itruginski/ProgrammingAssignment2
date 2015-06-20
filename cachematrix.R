## Use these functions together to...
##1. make a cached matrix (by passing makeCacheMatrix a matrix object), and then 
##2. calculate the inverse of that cached matrix object by using cacheSolve()

## makeCacheMatrix is a function that returns/stores a list of 4 functions

makeCacheMatrix <- function(x = matrix()) { #takes a matrix as argument
        i <- NULL 
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(solve) i <<- solve
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
} 


## cacheSolve is a function that returns the inverse of a matrix given a cached matrix.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) { #verifies exists and inverse is not null
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)#solve function takes the inverse
        x$setinv(i)
        i
        ## Return a matrix that is the inverse of 'x'
}

#heres an example if you want to try for yourself...
#c <- matrix(1:4,2,2)
#cachec <- makeCacheMatrix(c)
#cacheSolve(fooc) ##returns the inverse of your original matrix c

#if you want to change the original matrix, then do this...
#cachec$set(matrix(2,2,2))
#cachec$get() #returns the new cached matrix
