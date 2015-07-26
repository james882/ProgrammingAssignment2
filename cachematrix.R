
# The makeCacheMatrix function reads a matrix, stores it in a cache and a list of four functions associated with the matrix
# The four functions allow the matrix and its inverse to be read from and written to the cache

#The cacheSolve function works on objects created with the makeCacheMatrix function
#It checks if the inverse of the underlying matrix has been cached
#If so,it retrieves that object from the cache and returns it
#If not, it calculates the value of the inverse, and both returns the inverse and stores it in the cache, ready for return
#the next time the function is called





makeCacheMatrix <- function(x = matrix()) {

  s <- NULL
  # The four functions are:
  # set to write the original matrix
  # get to read the original matrix
  # setsolve to write the inverse of the matrix
  # getsolve to read the inverse of the matrix  
  
  # The functions may be called as 'names' of the list object. 
  # So if the makeCacheMatrix is used to create an object cache1 and m1 and m2 are matrices
  # then cache1$get(), cache1$getsolve,  cache1$set(m1) and cache1$set are all valid function calls
  
  ## The methods get, setsolve and getsolve are intended to called by the function cacheSolve  
  
  
  #Define the function set to update the original data in the cache
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  #Define the function get to return the matrix called when makeCacheMatrix was first created, or since updated using $set()
  get <- function() x
  
  #Define the function setsolve to set the value of the inverse of the matrix in the cache
  setsolve <- function(solved) s <<- solved
  
  #Define the function getsolve to return the value of the inverse of the matrix in the cache
  getsolve <- function() s
  
  #create the list to group all the functions
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}



cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    
  #return the value from the cache
  s <- x$getsolve()
  #check if the value retrieved from the cache has been changed from its initial null value
  # if it has changed return a message first
  if(!is.null(s)) {
    message("returning the value from the cache") 
  } else {
    # if the value in the cache is still null, calculate the inverse matrix and assign it to the cache
    message("calculating inverse, assigning to cache and printing on screen")
    data <- x$get()  #get the data from the cache
    s <- solve(data, ...)   #calculate the inverse
    x$setsolve(s)  #assign the inverse to the cache
  }
  s  #in either case, print the inverse matrix out on the screen
}
