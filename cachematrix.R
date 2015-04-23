
## The makeCacheMatrix function is to store a matrix and the inverse matrix
## It has four functions:
##   Set - Holds the matrix
##   Get - Returns the matrix
##   Setinverse - Stores the inverse matrix
##   Getinverse - Returns the inverse matrix

makeCacheMatrix <- function(mx = matrix()) {
        mxInv <- NULL
        set <- function(nMx) {
                mx <<- nMx
                mxInv <<- NULL
        }
        get <- function() mx
        setinverse <- function(inv) mxInv <<- inv
        getinverse <- function() mxInv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The cacheSolve function is to calculate the inverse matrix.
## If the inverse matrix is already stored in makeCacheMatrix it returns the 
## stored value. If not, it computes the inverse matrix and stores the result
## in makeCacheMatrix.

cacheSolve <- function(mx, ...) {
        mxInv <- mx$getinverse()
        if(!is.null(mxInv)) {
                message("getting cached data.")
                return(mxInv)
        }
        data <- mx$get()
        mxInv <- solve(data)
        mx$setinverse(mxInv)
        mxInv
}
