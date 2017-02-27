
#The makeCacheMatrix function contains the methods needed to set the value of a matrix, get the matrix, set its inverse and get its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv_x<-NULL
    setMatrix<-function(y){
        x<<-y
        inv_x<<-NULL
    }
    getMatrix<-function() x
    setInverse<-function(solve) inv_x<<-solve
    getInverse<-function() inv_x
    list(setMatrix=setMatrix, getMatrix=getMatrix, setInverse=setInverse, getInverse=getInverse)
}


## On passing the result of the above function to the cacheSolve function, it makes use of the methods defined in the above function in order to calculate the inverse of the matrix. If the inverse has already been calculated (inv_x is not null), then it retrieves the cached data; else it calculates and caches the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_x<-x$getInverse()
        if(!is.null(inv_x)) {
            message("getting cached data")
            return(inv_x)
        }
        mat<-x$getMatrix()
        inv_x<-solve(mat)
        x$setInverse(inv_x)
        return(inv_x)
}
