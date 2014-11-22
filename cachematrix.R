## These functions create a special matrix object and caches its inverse
## For this assignment, a square matrix is assumed. A second assumption 
## is that the matrix is always invertible. We will use the R solve function
## to compute the inverse of the matrix.


## The first function, makeCacheMatrix creates a special "matrix", which is 
## really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {

## initialize the value of the inverse
        
        inverse <- NULK
        
        ## set value of matrix and cache it (with <<- operator)
        
        set <- function (y){
                x <<- y
                inverse <<- NULL 
        }
        
        ## get matrix value
        
        get <- function() x
        
        ## set inverse and cache it
        
        setinverse <- function(solve) inverse <<- solve
        
        ## get inverse from cache
        
        getinverse <- function() inverse
        
        ## establish the operations list
        
        list (set = set, get = get, 
              setinverse = setinverse,
              getinverse = getinverse)
        }


## The following function calculates the inverse of the special "matrix"
## created with the above function.
## It first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and set the value 
## of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## first see if the inverse has already been computed by checking its
        ## presence in cache

        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached inverse")
                return (inverse)
        }
        
        ## otherwise, we have to compute the inverse (using the R
        ## function solve) and cache it
        
        my_matrix <- x$get()
        inverse <- solve(my_matrix)
        x$setinverse(inverse)
        inverse
}
