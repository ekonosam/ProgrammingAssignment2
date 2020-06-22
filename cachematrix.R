##  

## This script contains 2 functions that calculate the inverse of an matrix. 
## The functions make an efficient use of memory (caching objects) by taking advantage of the fact that R works through static scoping.


## The makeCacheMatrix (mCM) function contains in its enviroment a list of functions to cache the input matrix 
## or to modify it outside its enviroment. 
## Rewriting the values in the enviroment of the mCM function.

makeCacheMatrix<- function(x=matrix()){
    if (dim(x)[1]==dim(x)[2]){
        inv<-NULL     
        set_mat <- function(y){
            if(dim(y)[1]==dim(y)[2]){
                x <<- y 
                inv <<- NULL
            } else {
                stop("Error: Matrix is not invertible")
            } # Prevent the "cacheSolve" function from executing the error "The matrix must be square"
        }
        get_mat<- function() x
        set_inv<- function(inverse) inv <<- solve(inverse)
        get_inv<- function() inv
        list(set_mat= set_mat, get_mat=get_mat,set_inv=set_inv,get_inv=get_inv)
    } else {
        stop("Error: Matrix is not invertible")
    } # Prevent the "cacheSolve" function from executing the error "The matrix must be square"
}

## The cacheSolve function calculates the inverse of the matrix stored in the mCM function cache. 
## If a stored matrix already exists, it does not recalculate it.

cacheSolve<- function(x, ...) {
    inv<- x$get_inv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data<- x$get_mat()
    inv<- solve(x, ...)
    x$set_inv(inv)
    inv
}
