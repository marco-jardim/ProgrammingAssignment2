# makeCacheMatrix creates a list containing a function to


# required library by ginv() function, to invert M*N matrix
library(MASS)

# makeCacheMatrix creates a list with functions to get and set the inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(tempMatrix) {
        x <<- tempMatrix
        inv <<- NULL
    }
    get <- function() {
		x
	}
    setInverse <- function(inverse) {
		inv <<- inverse
	}
    getInverse <- function() { 
		inv
	}
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# If the matrix returned on getInverse() has already been computed, return the cache. If not, compute it
# return and store as cache
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        return(inv)
    }
    data <- x$get()
    inv <- ginv(data) # ginv() requires MASS library
    x$setInverse(inv)
    inv
}


# Sample run:
# randomMatrix <- matrix( rnorm(10000*1000,mean=0,sd=1), 10000, 1000)
# cache <- makeCacheMatrix(randomMatrix)
# cacheSolve(cache)
# cacheSolve(cache) ## should be faster because it's cached
