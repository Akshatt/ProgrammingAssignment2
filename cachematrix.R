## Caching Matrix Inverse

## This program will fetch inverse of a matrix faster for long matrices.
## It works by storing  the inverses of matrices created using 
## the special makeCacheMatrix function, which is the first function.

## This function will set the inverse of that matrix using solve()
## into the variable setinverse(). 

## Later when we want the inverse we call the second function which
## checks if the inverse is already in the cache using getinverse(). 
##If yes, then it returns the cached inverse. Otherwise, it creates 
## the inverse and stores it in setinverse() variable. 



## Args: 
##      x: A matrix 
## Returns:
##      Matrix with functions to get/set value and get/set inverse

makeCacheMatrix <- function(x = matrix()) {
    # set inverse of matrix to NULL
    n <- NULL
    
    # set/get matrix
    set <- function(y) {
        x <<- y
        n <<- NULL
    }
    get <- function() x 
    
    # get/set matrix inverse
    setinverse <- function(inv) n <<- inv    
    getinverse <- function() n
    
    # returns list of functions for matrix
    list(set=set,get=get, setinverse=setinverse, getinverse=getinverse)
}


## Args: 
##      x: A matrix 
##      ...: extra arguments
## Returns:
##      Inverse of Matrix.

cacheSolve <- function(x, ...) {
    # fetch cache inverse from makeCacheMatrix
    n <- x$getinverse()
    
    # return cached inverse matrix if its computed already. 
    if(!is.null(n)){
        message("Getting cached Inverse")
        return(n)
    }
    
    # else compute inverse for matrix 
    matrx <- x$get()
    inv <- solve(matrx,...)
    
    # set value of inverse matrix using setinverse 
    x$setinverse(inv)
    
    # return inverse matrix after setting it.
    n
}
