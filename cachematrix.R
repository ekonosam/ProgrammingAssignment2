## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix<- function(x){
    if (dim(x)[1]==dim(x)[2]){
        inv<-NULL     
        set_mat <- function(y){
            x<<-y 
        }
        get_mat<- function() x
        set_inv<- function(inverse) inv <<- solve(inverse)
        get_inv<- function() inv
        list(set_mat= set_mat, get_mat=get_mat,set_inv=set_inv,get_inv=get_inv)
    } else {
        print("Error: Matrix is not invertible")
    }
}


## Write a short comment describing this function

cacheSolve<- function(x, ...) {
    inv<- x$get_inv()
    if(!is.null(x)){
        message("getting cached data")
        return(inv)
    }
    data<- x$get_mat()
    inv<- solve(x, ...)
    x$set_inv(inv)
    inv
}