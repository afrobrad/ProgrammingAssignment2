

makeCacheMatrix <- function(x = matrix()) 
{
  
  ## ----------------------------------------------------
  ## makCacheMatrix function
  ##  
  ## B.MArtin 8 April 2020
  ## Derived from concept/code by Dr R.Peng, TJHU 
  ##
  ## Writen for R Programming course to demonstrate the  
  ## creation a special matrix and use of <<- operator to 
  ## assign values to global variables.
  ## Work in conjunction with CacheSolve function to store
  ## retrive an inverted matrix.
  ## Creates a list containing the function definitons
  ## Set and Get value of a matrix
  ## Set and Get inverse of a matrix
  ## ----------------------------------------------------
    
  m <- NULL
  set <- function(y) {
      x <<- y
      m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) 
{
  ## ----------------------------------------------------
  ## cacheSolve function
  ##  
  ## B.MArtin 8 April 2020
  ## Derived from concept/code by Dr R.Peng, TJHU 
  ##
  ## Writen for R Programming course to demonstrate caching 
  ## of a inverted matrix.  
  ## 
  ## Note : assumes x is square matrx. No error checking
  ## or handling included.
  ## ----------------------------------------------------

  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("Retrieving cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
