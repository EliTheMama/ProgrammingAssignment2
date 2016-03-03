#-------------------------------------------------------------------------#
#- This script contains 2 functions: makeCacheMatrix & cacheSolve        -#
#- -----------------------------------------------------------------------#
#- makeCacheMatrix will calculate the inverse of a matrix (NB: it assumes-#
#- the matrix is invertible) and store the result in the cache           -#
#-                                                                       -#
#- cacheSolve will calculate the inverse of the matrix if it has not     -#
#- already been computed. If it has already been computed, it will skip  -#
#- the computation and then retrieve the result from the cache.          -#                                                               ##
#-------------------------------------------------------------------------#


###########################################################################
## makeCacheMatrix is a function which will create a special matrix      ##
## object by the following:                                              ##
##                                                                       ##
## 1. set : Will set the value of the matrix                             ##
## 2. get : will get the value of the matrix                             ##
## 3. setinverse : will set the value of the inverse matrix              ##
## 4. getinverse : will get the value of the inverse matrix              ##
###########################################################################
makeCacheMatrix <- function(x = matrix()) {
    m_inv <- NULL
    set <- function(y) {
        x <<- y
        m_inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m_inv <<- inverse
    getinverse <- function() m_inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


###########################################################################
## cacheSolve is a function which will compute the inverse of a matrix if##
## not already computed, else it will calculate then store the result in ##
## the cache.                                                            ##
###########################################################################
cacheSolve <- function(x, ...) {
    m_inv <- x$getinverse()
    if(!is.null(m_inv)) {
        message("getting cached data")
        return(m_inv)
    }
    data <- x$get()
    m_inv <- solve(data)
    x$setinverse(m_inv)
    m_inv
}