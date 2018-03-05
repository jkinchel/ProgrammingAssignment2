#set default library path - where to look for packages, which = libraries
.libPaths("C:/Users/202999/OneDrive - City and County of Denver/R/win-library")
#set working directory
setwd("C:/Users/202999/OneDrive - City and County of Denver/R Coursera")


##create 2 x 2 matrix to pass to the function. This matrix has an inverse.

m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m1

#Create a "matrix" object that can cache its inverse. It will build a set of functions and return 
# set(), get(), setinverse(), and getinverse() within a list to the parent environment. It also 
#defines x and m in the parent environment 

#(x = matrix()) stipulates the default value of x is an empty matrix

makeCacheMatrix <- function(x = matrix()) {
  #set the value of m to null in this environment as a place holder to be replaced with another value
    m <- NULL
    
    #create a new function within the function to set(mutate) data values
    
    set <- function (y) {
      ##assigns the matrix y to the value of x in the parent enviroment 
    x <<- y
    
    ##assigns NULL to the value of m in the parent environment. 
    ##This clears any value of m that had been cached by a prior execution of cacheSolve()
    
    m <<- NULL
    }
    
    #retrieves the value of x from the parent directory, since x is not defined within the function get()
    
    get <- function() x
    
    #defines the setter and assigns the value of solve to m in the parent environment. It takes the inverse of the matrix and assigns it to m. 
    #In this case, they use solve as both a function and a variable just to mess with our heads
    
    setinverse <- function(solve) m <<- solve
    
    #returns the inverse of the matrix, m
    
    getinverse <- function() m
    
    #create a named list of functions to get and set both the original matrix
    #and its inverse, and return the list to the parent environment. 
    #Naming the list elements is what allows us to use the $ form of the extract 
    #operator to access the functions by name
    
    list (set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}

#create a new object to store the list returned by makeCacheMatrix

myMatrix_object <- makeCacheMatrix(m1)
print(myMatrix_object)

# Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then 
#cacheSolve will retrieve the inverse from the cache.


cacheSolve <- function(x) {

# Return a cached matrix that is the inverse of 'x', but only if m is not null. 
#If m is null, calculate the inverse of the matrix taken from the input object
# and return it to the parent environment when you print m
  
  m <- x$getinverse()
  if(!is.null(m)) {
        message("getting cashed data")
    return (m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}

#call the function cacheSolve for the object returned by makeCacheMatrix. 
#It returns the inverse of matrix x (m1) 

cacheSolve(myMatrix_object)
