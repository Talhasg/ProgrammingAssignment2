## Calculates inverse of a matrix and cache the value
## Next time when inverse is called, function first checks if matrix has changed. 
## if Matrix has changed it calculates inverse and cache value
## If Matrix has not changed it gets cached value and display

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(matrixI) inverse <<- matrixI
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse<-x$getinverse()
 # return(x$get())
  if(!identical(c,x$get())){ # If statement is checking if matrix has changed
  #    return(c)
  #    message("inside")
      x <-makeCacheMatrix(c)
      x$set(c)
      data<-x$get()
  #    return(data)
      inverse<-solve(data)
      x$setinverse(inverse)
      return(inverse)
  }
  if(!is.null(inverse)){
      message("getting cached matrix inverse")
      return(inverse)
  }  
  data<-x$get()
  inverse<-solve(data)
  x$setinverse(inverse)
  inverse
}
