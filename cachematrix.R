## The functions below implement together a cached form of a matrix with its inverse
## makeCacheMatrix creates the special matrix that caches its inverse
## cacheSolve computes the inverse of above said matrix (if not already done)
## and returns its result (plus stores it in the matrix, if necessary)


## makeCacheMatrix implements a matrix, its cached inverse 
## and the get/set functions belonging to it, using <<- to store the values

makeCacheMatrix <- function(x = matrix()) { 
        ##init inverse matrix
        s<-NULL
        ## declare function set()
        set<-function(y){
              x<<-y
              s<<-NULL
        }
        ## declare function get()
        get<-function() x
        ## declare function setsolve()
        setsolve<-function(solve) s<<-solve
        ## declare function getsolve()
        getsolve<-function() s
        ## return the four functions in a list
        list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)

}


## cacheSolve is needed to calculate the inverse of the cachematrix
## it uses the R solve() so for help with parameters look at ?solve 
## returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        ## get the inverse of matrix x
        s <- x$getsolve()
        ## if available, return value and break out of the function
        if (!is.null(s)){
                return(s) #cached data
        }
        ## if the inverse is not available, get the matrix
        ## solve it, store the inverse, return the inverse
        data <- x$get()
        s <-solve(data,...)
        x$setsolve(s)
        s
}
