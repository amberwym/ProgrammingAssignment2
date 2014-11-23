## The first function, makeCacheMatrix creates a special "matrix" that can cache its inverse


## The function of makeCacheMatrix is a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function()
                x
        setinvert<-function(solve) 
                m<<-solve
        getinvert<<-function()
                m
        list(set=set ,get= get,
             setinvert = setinvert,
             getinvert = getinvert)

}


## This second function, cacheSolve, computes the inverse of the special "matrix"
## returned by makeCacheMatrix. If the inverse has already been calculated (and the
## matrix has not changed), then the cacheSolve should retrive the inverse from the cache.

cacheSolve <- function(x) {
       m<-x$getinvert()
       if(!is.null(m)){
               message("getting cached data")
               return(m)
       }
       data<-x$get()
       m<-solve(data)
       x$setinvert(m)
       m
}


