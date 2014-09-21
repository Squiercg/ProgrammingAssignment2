## Put comments here that give an overall description of what your
## functions do

# Matrix inversion is usually a costly computation save partial results for future
#use might be a good idea

## Write a short comment describing this function
# This function creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    #start things
    inverso <- NULL
    set <- function(y) {
        x <<- y
        inverso <<- NULL
    }

    #Compute inverso
    setinverse <- function(inverse) inverso <<- inverse

    #Getter for input and inverso
    get <- function() x
    getinverse <- function() inverso

    #Output
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)

}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix. If its already used and the data have not changed,
#the result will be on cache

cacheSolve <- function(x, ...) {

    #Get inverse value (Can be null)
    inv <- x$getinverse()

    #Ask if the computation was already done
    if(!is.null(inv)) {
        #If True, just return it
        message("getting cached data")
        return(inv)
    } else {
        #If not True, make the calculation and store it
        #Get original data.
        data <- x$get()
        #Computation
        inv <- solve(data, ...)
        #Cache
        x$setinverse(inv)
        #Returb
        return(inv)
    }
}


#Example
matriz<-matrix(c(2,3,2,2),2,2)

#cache
matrizcache<-makeCacheMatrix(matriz)
#first solve
matrizsolve<-cacheSolve(matrizcache)
#retrieve data
matrizsolve2<-cacheSolve(matrizcache)
#retrieve data
matrizsolve3<-cacheSolve(matrizcache)
#Multiplication of both
round(matriz%*%matrizsolve)
