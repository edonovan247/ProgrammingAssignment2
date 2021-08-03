##functions compute inverse of a square matrix and cache result

##This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set <- function(y) {
        x<<-y
        inv<<-NULL
    }
    get<-function() x
    setinv<-function(inverse) inv<<-inverse
    getinv<-function() inv
    list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## This function computes the inverse of the special "matrix" object
## created by the makeCacheMatrix function, first looking in the cache
## to retrieve the inverse if it has already been calculated

cacheSolve <- function(x, ...) {
    inv<-x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    temp<-x$get()
    inv<-solve(temp,...)
    x$setinv(inv)
    inv
}