## Second R Programming Assignment
## These functions caches the result of a matrix inverse computation
## which can be a time consuming computation.  In this way once an inverse is 
## computed it can be re-used in future computations without the need to 
## recalculate
## The two function swork together to create a special object that stores a matrix
## and caches its inverse

## The first function makeCacheMatrix, creates a list containing functions that
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse of the matrix
## 4. Get the value of the inverse of the matrix


## Calculate and Cache inverse of matrix x
makeCacheMatrix <- function(x = matrix()) {
      
      set <- function (y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) inv <<- solve
      getinverse <- function() inv
      list (set = set, get = get, 
            setinverse = setinverse,
            getinverse = getinverse)
}


## Compute the inverse of matrix x.
## If the inverse has already been calculated, return the
## inverse from the cache.  Otherwise calculate inverse 
## and update cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
        my_inv <- NULL
        
        cm <- makeCacheMatrix(x)
        
        my_inv <- cm$getinverse()
          
      
        if (!is.null(my_inv)) {
              message ("getting cached data")
              return(my_inv)    ## return inverse from cache
        }

        data <- cm$get()
        inv <- solve(data, ...)
        cm$setinverse(inv)
        inv                     ## calculate and return inverse
}
