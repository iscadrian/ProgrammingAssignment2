## Tow functions that work in conjunction to create a matrix and
## calculate its inverse. It caches the inverse of the matrix if it is 
## calculated already


## makeCacheMatrix function to create a matrix and caches its inverse.
## Get and Set functions are defined here

makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse matrix as NULL
  im <- NULL
  
  # Set the matrix in the working environment
  set <-function(y){
    x<<-y
    im<<-NULL
  }
  
  ## Return the matrix
  get<-function() x
  
  ##Set the inverse of the matrix in cache
  setinverse<-function(inverse_matrix) im <<- inverse_matrix
  
  ##Get the inverse of the mareix
  getinverse<-function() im
  
  ## Return the functions to the working environment
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve calculates the inverse of the matrix using the makeCacheMatrix function
## If inverse matrix is calculated already, it returns the cache value


cacheSolve <- function(x, ...) {
  
  ## Get the inverse of the matrix 
  im<-x$getinverse()
  
  ## Validates if it is already calculated.If so, it returns the message "getting cached data"
  if(!is.null(im)){
    message("getting cached data")
    return(im)
  }
  
  ##Get the oroginal matrix
  data <-x$get()
  
  ##Calculates the inverse of the matrix
  im<-solve(data)
  
  ##Set the inverse of the matrix in cache
  x$setinverse(im)
  
  ##Inverse of the matrix as the output
  im
}
