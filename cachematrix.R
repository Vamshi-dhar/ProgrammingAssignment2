#This function will helps in a scenario where, we needed a "huge same data" to be 
#repeatedly get computed in a Loop or somthing. Therefore its better to cache 
#the value and use it from previously computed value, rather than recomputing 
#it every time.


#makeCacheMatrix: 

#Has four main function that aids in:
#seting a input value within the parent environment, using set function

#makeing input available to parent frame when cacheSolve function called, using 
#get function

#seting computed value to specified object, using set_inverse function.

#assigning inverse symbol to computed output using "<<-" so that it can be availabel 
#to next function when called, using get_inverse function

#note:
#Given: For this assignment, assume that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) { #Input: Square Matrix
  inverse <- NULL
  set <- function(y) {                      #setting input matrix:
    x <<- y
    inverse <<- NULL
  }
  get <- function() x 
  set_inverse <- function(a) inverse <<- a
  get_inverse <- function() inverse
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}
#Cache data if it is computed previously:
cacheSolve <- function(x, ...) {
  inverse <- x$get_inverse()
  if(!is.null(inverse)) {                   # If solve function already executed, then cache data. 
    message("Getting cached data")
    return(inverse)                  
  }
  data <- x$get()                           #Recieve data from the parent enveronment, makeCacheMatrix function
  inverse <- solve(data, ...)              
  x$set_inverse(inverse)                    #calling set_inverse(), to assign inverse symbol to output of solve()
  inverse
}

#Twesting above functions:
#Inverse of matrix:
c=rbind(c(4, 7), c(2, 6))  
v <- makeCacheMatrix(c)
cacheSolve(v)
