#helper functions for setting and getting the solution
#of a matrix
makeCacheMatrix <- function(x = matrix()) {
  soln <- NULL
  set <- function(y) {
    x <<- y
    soln <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setsoln <- function(newSoln) {
    soln <<- newSoln
  }
  
  getsoln <- function() {
    soln
  }
  
  out <- c(set = set, 
           get = get, 
           setsoln = setsoln,
           getsoln = getsoln)
  matrix(out)
}

#if the solution of a matrix is not set,
#calulate the matrix solution and set it
#if it is saved, access the saved solution

#cacheSolve takes as its argument x, the helper function
#the helper function takes the matrix as its argument
cacheSolve <- function(x, ...) {
  #x refers to the matrix returned by the helper function
  #it is a matrix containg function calls
  m <- x[[4]]() #calls getsoln()
  if(!is.null(m))  {
    message("getting cached data")
    return(m)
  }
  data <- x[[2]]() #calls get()
  #if no matrix is specified to the helper function, 
  #the function defaults to an empty matrix
  m <- solve(data)
  x[[3]](m) #calls setsoln()
  #returns matrix solution
  m
}