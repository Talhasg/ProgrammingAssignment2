## Calculates inverse of a matrix and cache the value
## Next time when inverse is called, function first checks if matrix has changed. 
## if Matrix has changed it calculates inverse and cache value
## If Matrix has not changed it gets cached value and display

## This function return a list with function to set and get matrix and its inverse

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

## This functions returns matrix inverse. It first checks if inverse is cached and matrix
# has not changed. Recomputes inverse
## if inverse does not exist or if matrix has changed

cacheSolve <- function(x, ...) {

  inverse<-x$getinverse()

  if(!identical(c,x$get())){ # If statement is checking if matrix has changed
      message("inside")
      x<<-makeCacheMatrix(c)
      x$set(c)
      data<-x$get()
      inverse<-solve(data)
      x$setinverse(inverse)
      return(inverse)
  }

  if(!is.null(inverse)){
      inverse<-x$getinverse()
      message("getting cached matrix inverse")
      return(inverse)
  }  
  data<-x$get()
  inverse<-solve(data)
  x$setinverse(inverse)
  inverse
}
