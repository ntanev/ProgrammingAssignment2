
## This function creates a special "matrix" object that can cache its inverse. 
## First, objects are initialized:

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
## Then, the “getters and setters” are defined, i.e. the modules which obtain and transform the data 
## within an object:
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
## Lastly, each of the functions are assigned to an element within a list 
## I gave the names ‘set and ’'get' to the set() and get() functions defined previously allowing the access ## by name to the functions
## I gave the names 'setInverse' and ‘getInverse’ to the setInverse() and getInverse() functios defined 
## above allowing the access by name to the functions
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv  
}
