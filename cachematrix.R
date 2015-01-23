## Function that caches the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {

        # Initialization of the inverse matrix to null
        i <- NULL

        # Definition of set as a function which stores a new source matrix and
        # initializes its inverse to null
        set <- function(y) {
                x <<- y
                i <<- NULL
        }

        # Definition of get as a function that returns the current source matrix
        get <- function() x

        # Definition of setinverse as a function that stores in i the inverse matrix that it is passed
        setinverse <- function(inverse) i <<- inverse

        # Definition of getinverse as a function that returns the stored inverse matrix.
        getinverse <- function() i

        # List with the functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Function that returns the inverse of a matrix. If the inverse of the matrix
##    had been calculated previously, it returns if from the cache, otherwise
##    it calculates the inverse of the matrix before returning it

cacheSolve <- function(x, ...) {
        # Try to get the inverse matrix
        i <- x$getinverse()

        # If i is not null it means that it had been calculated previously, so we return i.
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        # Otherwise it means that the inverse was not still calculated. First we get the data.        
        data <- x$get()

        # We call solve to calculate the inverse
        i <- solve(data,...)

        # We store the inverse matrix in the cache
        x$setinverse(i)

        # We return the inverse matrix
        i
}
