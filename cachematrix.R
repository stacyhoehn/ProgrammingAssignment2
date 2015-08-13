## makeCacheMatrix is a function that takes a matrix as its input and
## outputs a list of 4 functions that
## 1) set the value of a matrix to a specified value,
## 2) get the value of a matrix,
## 3) set the variable inv to a specified value, and
## 4) get the value of the variable inv 

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL #initializes the value of inv to NULL
 
 #The set function substitutes the input matrix y in 
 #for the matrix x and resets the inv variable to NULL
 #in the main function makeCacheMatrix 
 set <- function(y) {
   x <<- y
   inv <<- NULL
 }
 
 #The setinv function changes the value of the inv variable in the main
 #function makeCacheMatrix to whatever matrix is given as input to this 
 #function.  Note that inverse is simply a user-specified value; it may
 #not actually be the inverse matrix.
 setinv <- function(inverse) inv <<- inverse
 
 #The get function simply displays the current value of the matrix x;
 get <- function() x
 
 #The getinv function simply displays the current value of the variable inv;
 getinv <- function() inv
 
 #returns a list containing all 4 functions
 list(set = set, get=get, setinv=setinv,getinv=getinv)
 }



## cacheSolve is a function that takes as its input a list
## of the type returned by makeCacheMatrix and returns the inverse
## of the associated matrix.  If this inverse has already been computed,
## the value is retrieved from the cache instead of recomputing it.

cacheSolve <- function(x, ...) {

  inv <- x$getinv() #determines what is currently stored in the inv variable
  
  #If the inverse has already been computed, obtain its value from the cache
  #and then exit this function
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  #Otherwise, obtain the value of the matrix and find its inverse, storing 
  #this value in the cache for future reference
  matrix <- x$get()
  inv <- solve(matrix,...)
  x$setinv(inv)
  inv
  }
  
