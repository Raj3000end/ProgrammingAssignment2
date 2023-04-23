## Functions that cache the inverse of matrix

## create a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL  # initialize the inverse property
   set <- function(y) {     # method to set the matrix 
     
     x <<- y
     inv <<- NULL
     
   }
    get <- function() x   # method to get the matrix
    setinverse <- function(solvematrix) inv <<- solvematrix    #method to set the inverse of matrix 
    getinverse <- function() inv     # method to get the inverse of the matrix 
    
    list( set=set, get=get,    # return a list of methods 
          setinverse = setinverse, 
          getinverse = getinverse)
    
}


## compute the inverse of the special matrix returned by makecachematrix, if the inverse
## has already been calculated and the matrix has not been changed then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse() # return a matrix that is inverse of 'x'
  if(!is.null(inv)){  # if inv is not null it will show message and get cached inverse matrix
    message("getting cached inverse matrix")
    return(inv)
  }
  data <- x$get()  # get the matrix from our object
  inv <- solve(data, ...) # calculate the inverse using matrix multiplication
  x$setinverse(inv) # set the inverse to the object 
  inv # return the matrix 
  
}
