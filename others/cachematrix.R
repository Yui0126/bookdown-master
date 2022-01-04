## Function`makeCacheMatrix` and `cacheSolve` work hand in hand to get and cache inverse of matrices.
## Computing inverse of matrices can sometimes be time and memory consuming for computers,
## but if we use the functions, the inverse can be retrieved from parent environment, 
## making computation more efficient. 
## Yui Naruse 5/7/2021

## `makeCacheMatrix` allows us to set and cache matrix inverse.
## The function defines the default of input value as matrix and 
## allows computed values to be cached in parent environment using `<<-` operator. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## This `cacheSolve` function returns the inverse of matrix defined with function created above.
## If the inverse value is already cached in the parent environment, it returns that value without calculating the inverse again.
## If not, it will compute the inverse and set it as new inverse value.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


## Trying out the functions
myMatrix <- makeCacheMatrix(x = matrix(c(3,5,7,12,14,16,21,25,
                                         29,33,38,62,84,72,98,100),
                                       4,4))
cacheSolve(myMatrix) # if i run this code for the second time, I will get the cached inverse with message "getting cached data".
