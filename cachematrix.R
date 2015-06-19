## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## ## return: a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##         this list is used as the input to cacheSolve()
makeCacheMatrix <- function(x = matrix()) { 
  # begins by setting the mean to NULL as a placeholder for a future value
   m <- NULL  
  
  #define a function to set the matrix, x, to a new matrix, y, and resets the mean, m, to NULL
  set <- function(y)
  {
  #cache inputed matrix so the other function can check if it is changed
  x <<- y 
  #sets the value of m to NULL so (matrix inverse if used cacheSolve)
  m <<- NULL 
  }
  
  # returns the cache matrix, x
  get <- function() x    
  
  # sets the inverse, m, to inverse, to inverse the matrix
  setinverse <- function(inverse) m <<- inverse     
  
  #returns the inverse, m
  getinverse <- function() m  
  
  #returns the 'special matrix' object containing all of the functions just defined
  list(set = set, get = get,                
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve return: inverse of the original matrix input to makeCacheMatrix()
#check if an inverse has already been calculated ifso then return  
#if not compute value of the inverse of the input matrix 

cacheSolve <- function(x, ...) {

  #get the matrixx from getinverse, input was list of functions
m <- x$getinverse()

# check to see if cacheSolve/inverse has been run before
if(is.null(m)) { 
  
   #send a text message and return the cached matrix
    message("getting cached data")
    return(m)
} 
    # otherwise 
    # run the get function (x$get to retrieve from list) 
    #to get the value of the not yet inversed input matrix
    #input matrix is assigned to y after retrieved from input matrix
    y <- x$get() 
    
    # run the set function on the input matrix to cache it
    x$set(y) 
    
    # compute the value of the inverse of the input matrix
    m <- inverse(y, ...)
    
    #run the setinverse function on the matrix m to 
    #set the value of the inverse in the cache 
    x$setinverse(m) 
    return(m) # return the inverse  
}
