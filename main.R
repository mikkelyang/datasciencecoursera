#library(swirl)
#install_from_swirl("R Programming")

makeCacheMatrix <- function(x,nrow, ncol){
        mat <- matrix(x, nrow, ncol)
        inv <- NA
        get <- function () mat
        set_inverse <- function (inverse) inv <<- inverse
        get_inverse <- function () inv
        list(get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}

cacheSolve <- function (x, ...){
        inv <- x$get_inverse()
        if(!is.na(inv)){
                print('Getting from Cache')
                return(inv)
        }
        data <- x$get()
        comp_inv <- solve(data)
        x$set_inverse(comp_inv)
        comp_inv
}

my_matrix <- makeCacheMatrix(c(3, 3.2, 3.5, 3.6), nrow=2, ncol=2)
cacheSolve(my_matrix)