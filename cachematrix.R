## Programming Task Week 2
## Functions to calculate the inverse matrix and store it for further use

makeCacheMatrix <- function(mx = matrix()) {
  # Function creating an R-object storing a matrix (mx), its inverse matrix (invmx
  # as well as functions for accessing and storing these matrices
  
  invmx <- NULL
  
  set <- function(y){
    mx <<- y
    invmx <<- NULL              # Ensuring calculation of new inverse matrix
  }
  get <- function () mx
  
  setsolve  <- function (solvedmx) {
    invmx <<- solvedmx
  }
  
  getsolve  <- function () invmx
  
  list(set=set,get=get,setsolve=setsolve,getsolve=getsolve) # return value 
} # End makeCacheMatrix

cacheSolve <- function(x, ...) {
  # Function returning the inverse matrix of x, newly calculated or,
  # if available, a previously calculated 
  
  invmx<-x$getsolve()
  
  if(!is.null(invmx)){       
    # Use cached inverse matrix
    message("getting cached data")
    invmx<-x$getsolve();  
  }
  else {
    # calculate inverse matrix
    message("calculation of inverse matrix")
    data <- x$get()
    invmx <- solve(data,...)  #use standard function for inverse matrix calculation
    x$setsolve(invmx)
  }

    return(invmx)

} # End cacheSolve