## In this assignment we use the <<- operator which can be used to assign 
## a value to an object in an environment that is different from the 
## current environment. Below are the two functions that are used to create 
## a special object that stores a matrix and caches its inverse. However, while
## supplying the matrix it is important to supply only invertible matrix 
## (one of the conditions is that an invertible matrix is always a square matrix, however, 
## not all square matrices are invertible).  


## The function stores the matrix supplied to it and creates a special matrix, 
## which contains the list of a function to
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse of matrix
## 4. Get the value of the inverse of matrix 

makeCacheMatrix <- function(x = matrix()) {
			I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) I <<- inverse
        getinverse <- function() I
        list(set = set, get = get,
             setinverse = setinverse,
            getinverse = getinverse)
 }


## The following function computes the inverse of special "matrix" returned by
## the above function makeCacheMatrix. However, it first checks 
## to see if the inverse of the matrix has already been calculated. If so, it 
## gets the inverse from the cache and skips the computation. Otherwise, it  
## calculates the inverse of the matrix and sets the value of the inverse in  
## the cache via the setinverse function.If it is calculating the inverse, it 
## will display the message "First time calculating inverse matrix" and if it is
## getting the cached inverse matrix, it'll display "Getting cached inverse matrix"

cacheSolve <- function(x, ...) {
  	  I <- x$getinverse()
        if(!is.null(I)) {
                message("Getting cached inverse matrix")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setinverse(I)
	  message("First time calculating inverse matrix")
        I      
 }

 
