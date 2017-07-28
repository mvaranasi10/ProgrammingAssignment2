# Functions create the inverse of the matrix and cache the result so that it can be used and recalculated everytime

# Information on Inverse of Matrix and how to calculate can be seen here - https://www.mathsisfun.com/algebra/matrix-inverse.html
# Inverse of Matrix in R is calculated using the solve function

# makeCacheMatrix Function has functions set/get/setInverse/getInverse

# cacheSolve Function calls the makeCacheMatrix Function to get the Inverse Matrix Value if existing or 
#     call the Set, calculate Inverse and setInverse functions

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
        
  # Create function set  
  set <- function(y) { 
    # << is used since x and inv are declared outside this function    
    x <<- y    
    inv <<- NULL  
  }   
        
  # Create function get  
  get <- function() { 
    x  
  }    
  
  # Create function setInverse    
  setInverse <- function(invval) { 
    # << is used since inv is declared outside this function 
    inv <<- invval  
  }    
  
  # Create function getInverse  
  getInverse <- function() { 
    inv  
  }    
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {  
  # Get the inverse value if existing    
  inv <- x$getInverse()
 
  # Check in the inv value is NULL/NOT NULL and get inv if NOT NULL    
  if(!is.null(inv)) {        
    message("getting cached data.")        
    return(inv)    
  }
 
  # Get the input Matrix    
  data <- x$get()
        
  # Calculate Inverse of the Matrix    
  inv <- solve(data)
 
  # Set the value of inv    
  x$setInverse(inv)
 
  # Return inv - Since it is the last statement    
  inv
}

# Testing the above functions
# > x = rbind(c(1, -1/4), c(-1/4, 1))
# > m = makeCacheMatrix(x)
# > m$get()      
#      [,1]  [,2]
#[1,]  1.00 -0.25
#[2,] -0.25  1.00

#> cacheSolve(m)          
#      [,1]      [,2]
#[1,] 1.0666667 0.2666667
#[2,] 0.2666667 1.0666667

#> cacheSolve(m)
#getting cached data.          
#      [,1]      [,2]
#[1,] 1.0666667 0.2666667
#[2,] 0.2666667 1.0666667
