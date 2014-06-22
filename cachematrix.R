
## This function creates a special "vector" object that can cache the  inverse of a matrix.
## The matrix must be a square matrix and should be input into the makeCachMatrix function as a vector listing the values column by column
## i.e. column 1 values, then column 2 values ....

## makeCachMatrix does the following

##    set the value of the vector that has the values of the matrix
##   get the value of the vector that has the values of the matrix
##    set the value of the inverse the matrix
##   get the value of the inverse of the matrix



makeCacheMatrix <- function(x = numeric()) {

	m <- NULL
        set <- function(x) {
                x <<- x
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
	}




## This function checks for the existance of the cached value of the matrix
## if a cached value exists it returns that value and states that it is the cached value
## if there is no cached value it calculates the inverse of the matrix
## note this function converts the vector representation of teh matrix into a square matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'


        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        datavector <- x$get()
	data <- matrix(datavector,ncol=sqrt(length(datavector)))
        m <- solve(data, ...)
        x$setinv(m)
        m
}

