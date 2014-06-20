
## This function creates a special "vector", which is really a list to:
## 1) set the value of a matrix
## 2) get the value of a matrix
## 3) set the inverse of a matrix
## 4) get the inverse of a matrix

## How to run:
## 1) x<-makeCacheMatrix() - stores function in a variable
## 2) x$set(matrix(1:4,2,2)) - sets matrix
## 3) cacheSolve(x) - run function to return inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}


## This function returns the inverse of a matrix. However it first checks to 
## see if the inverse has already been calculated.

cacheSolve <- function(x=matrix(), ...) {
        m<-x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
        
}
