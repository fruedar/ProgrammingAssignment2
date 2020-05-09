## Create a special "matrix object which stores it´s inverse in cache and can
## be retrieved when needed, skipping it´s computation.


## Creates a special "matrix" object containing a function to: 1.Set the value 
## of the matrix, 2.Get the value of the matrix, 3.Set the value of the Inverse 
## and 4.Get the value of the Inverse.  

makeCacheMatrix <- function(x = matrix()) {
    i<-NULL
    set<-function(y){
        x<<-y
        i<<-NULL
    }
    get<-function() x
    setInverse<-function(inv) i<<-inv
    getInverse<-function() i
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated, then it retrieves the inverse 
## from cache. Otherwise, the inverse is calculated and stored in cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    i<-x$getInverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data<-x$get()
    i<-solve(data,...)
    x$setInverse(i)
    i
}
