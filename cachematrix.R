## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## function to cache th inverse of a matrix
        
        m <- NULL
        #define funtion to set the value
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        #define function to get the value
        get <- function() x
        #set solve sets a pointer to the functio solve
        setsolve <- function(solve) m <<- solve()
        #getsolve returns the values which has been stored in m
        getsolve <- function() m
        #return a list that has the function to set, the funtion to get
        #the function to setsolve and 
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## casheSolve This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been calculated
##(and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #check to see if the inverse has already been calculated
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        #If it has not been calculated get the data to calculate with
        data <- x$get()
        #calculate the inverse
        m <- solve(data, ...)
        #store the inverse
        x$setsolve(m)
        #return the inverse
        m
}
