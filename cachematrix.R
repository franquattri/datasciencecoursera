
# write A PAIR OF FUNCTIONS THAT CACHE THE INVERSE OF A MATRIX

# makeCacheMatrix : Create a special "matrix" (here called "X") 
#that can cache its inverse. The matrix is assumed to always be invertible

# how do you compute the inverse of a square matrix? through the solve() function
# if X = square invertible matrix
# solve(X) = returns the inverse of X

makeCacheMatrix <- function(X = matrix()) {
  inverse <- NULL
  set <- function(y) { # set the value of the matrix
    X <<- y
    inverse <<- NULL
  }
  
  get <- function() X # get the value of the matrix
  setinverse <- function(inverse) inverse <<- inverse  # set the value of inverse 
  getinverse <- function() inverse # get the value of the inverse
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}



# cacheSolve: Create a function that compute the inverse of the special matrix 
# if the inverse has already been calculated and the matrix has not changed, 
# cachesolve should retrieve the inverse from the cache 


cacheSolve <- function(X, ...) {
  inverse <- X$getinverse() # get the value of inverse if already calculated
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)        
  }
  data <- X$get()
  inverse <- solve(data, ...) # calculate the value of inverse through the function solve()
  X$setinverse(inverse)
  inverse   # return the value of inverse 
}