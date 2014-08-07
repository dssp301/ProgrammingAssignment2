## These functions will be able to cache potentially time-consuming computations. 
## For example, taking the mean of a numeric vector is typically a fast operation.
## However, for a very long vector, it may take too long to compute the mean, 
## especially if it has to be computed repeatedly (e.g. in a loop). If the contents 
## of a vector are not changing, it may make sense to cache the value of the mean 
## so that when we need it again, it can be looked up in the cache rather than 
## recomputed.


# Define function makeCachematrix which will be capable of creating a cache for a particular matrix
# ... Assumption: all supplied matricies are invertible.  This also implies that all matricies are square
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # define the setter.  usage: M$set(matrix(...))   -Will cache the matrix in the parent's environment
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # define the get method.  usage: M$get()  -Will retrieve the matrix data
    get <- function() x
    # define the setsolve method into the parent environemnt
    setsolve <- function(solve) m <<- solve
    # define the get solve method to retrieve the solution from the parent envioronment
    getsolve <- function() m
    # sets the list of methods associated with the matrix variable
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}



# Define caching function for caching an inverse of a matrix
# ... Assumption: all supplied matricies are invertible
cacheSolve <- function(x, ...) {   # the '...' will allow passing of additional argumants to solve()
    # If we already have a cached inverse then return that
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # otherwise retrieve the original matrix and return the inverse using solve()
    data <- x$get()
    # find the inverse of the matrix 
    m <- solve(data, ...)
    # save the newly computed inverse in to the cache
    x$setsolve(m)
    # return the matrix object
    m
}
